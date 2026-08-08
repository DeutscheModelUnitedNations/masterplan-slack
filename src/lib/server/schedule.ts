import type { RowDataPacket } from 'mysql2';
import { bestSlackMatch, matchPersonByEmail, matchQuality, type PersonMatch } from '$lib/matching';
import type { Group, Location, MessageEntry, Person, ScheduleDay, ScheduleItem, SlackUser } from '$lib/types';
import { db } from './db';

// ---- Personen -------------------------------------------------------------

export async function listPeople(): Promise<Person[]> {
	const pool = await db();
	const [rows] = await pool.query<RowDataPacket[]>('SELECT id, name, email FROM people ORDER BY name');
	return rows.map((r) => ({ id: r.id, name: r.name, email: r.email }));
}

export async function createPerson(name: string, email: string | null): Promise<Person> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO people (name, email) VALUES (?, ?)',
		[name, email || null]
	);
	return { id: result.insertId, name, email };
}

export async function updatePerson(id: number, name: string, email: string | null): Promise<void> {
	const pool = await db();
	await pool.execute('UPDATE people SET name = ?, email = ? WHERE id = ?', [name, email || null, id]);
}

export async function deletePerson(id: number): Promise<void> {
	const pool = await db();
	await pool.execute('DELETE FROM people WHERE id = ?', [id]);
}

export async function findPersonForEmail(email: string): Promise<PersonMatch> {
	const people = await listPeople();
	return matchPersonByEmail(email, people);
}

// ---- Locations (fuer die OSM-Karte) -----------------------------------------

export async function listLocations(): Promise<Location[]> {
	const pool = await db();
	const [rows] = await pool.query<RowDataPacket[]>('SELECT id, name, lat, lng FROM locations ORDER BY name');
	return rows.map((r) => ({ id: r.id, name: r.name, lat: r.lat, lng: r.lng }));
}

export async function createLocation(name: string, lat: number | null, lng: number | null): Promise<Location> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO locations (name, lat, lng) VALUES (?, ?, ?)',
		[name, lat, lng]
	);
	return { id: result.insertId, name, lat, lng };
}

export async function updateLocation(id: number, name: string, lat: number | null, lng: number | null): Promise<void> {
	const pool = await db();
	await pool.execute('UPDATE locations SET name = ?, lat = ?, lng = ? WHERE id = ?', [name, lat, lng, id]);
}

export async function deleteLocation(id: number): Promise<void> {
	const pool = await db();
	await pool.execute('DELETE FROM locations WHERE id = ?', [id]);
}

// ---- Gruppen (bequemes Bulk-Zuordnen mehrerer Personen) ----------------------

export async function listGroups(): Promise<Group[]> {
	const pool = await db();
	const [groups] = await pool.query<RowDataPacket[]>('SELECT id, name FROM person_groups ORDER BY name');
	if (groups.length === 0) return [];

	const [members] = await pool.query<RowDataPacket[]>(
		'SELECT group_id, person_id FROM person_group_members WHERE group_id IN (?)',
		[groups.map((g) => g.id)]
	);
	const byGroup = new Map<number, number[]>();
	for (const m of members) {
		const list = byGroup.get(m.group_id) ?? [];
		list.push(m.person_id);
		byGroup.set(m.group_id, list);
	}
	return groups.map((g) => ({ id: g.id, name: g.name, personIds: byGroup.get(g.id) ?? [] }));
}

export async function createGroup(name: string): Promise<Group> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO person_groups (name) VALUES (?)',
		[name]
	);
	return { id: result.insertId, name, personIds: [] };
}

export async function renameGroup(id: number, name: string): Promise<void> {
	const pool = await db();
	await pool.execute('UPDATE person_groups SET name = ? WHERE id = ?', [name, id]);
}

export async function deleteGroup(id: number): Promise<void> {
	const pool = await db();
	await pool.execute('DELETE FROM person_groups WHERE id = ?', [id]);
}

export async function setGroupMember(groupId: number, personId: number, member: boolean): Promise<void> {
	const pool = await db();
	if (member) {
		await pool.execute('INSERT IGNORE INTO person_group_members (group_id, person_id) VALUES (?, ?)', [
			groupId,
			personId
		]);
	} else {
		await pool.execute('DELETE FROM person_group_members WHERE group_id = ? AND person_id = ?', [
			groupId,
			personId
		]);
	}
}

// ---- Tage -------------------------------------------------------------------

export async function listDays(): Promise<ScheduleDay[]> {
	const pool = await db();
	const [rows] = await pool.query<RowDataPacket[]>(
		'SELECT id, label, sort_order FROM schedule_days ORDER BY sort_order, id'
	);
	return rows.map((r) => ({ id: r.id, label: r.label, sortOrder: r.sort_order }));
}

export async function createDay(label: string): Promise<ScheduleDay> {
	const pool = await db();
	const [[{ maxOrder }]] = await pool.query<RowDataPacket[]>(
		'SELECT COALESCE(MAX(sort_order), -1) + 1 AS maxOrder FROM schedule_days'
	);
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO schedule_days (label, sort_order) VALUES (?, ?)',
		[label, maxOrder]
	);
	return { id: result.insertId, label, sortOrder: maxOrder };
}

export async function deleteDay(id: number): Promise<void> {
	const pool = await db();
	await pool.execute('DELETE FROM schedule_days WHERE id = ?', [id]);
}

// ---- Programmpunkte + Zuordnungen -------------------------------------------

export async function listItems(dayId: number): Promise<ScheduleItem[]> {
	const pool = await db();
	const [items] = await pool.query<RowDataPacket[]>(
		`SELECT si.id, si.day_id, si.time, si.title, si.team_info, si.sort_order,
		        l.id AS loc_id, l.name AS loc_name, l.lat AS loc_lat, l.lng AS loc_lng
		 FROM schedule_items si
		 LEFT JOIN locations l ON l.id = si.location_id
		 WHERE si.day_id = ?
		 ORDER BY si.sort_order, si.id`,
		[dayId]
	);
	if (items.length === 0) return [];

	const [assignments] = await pool.query<RowDataPacket[]>(
		'SELECT item_id, person_id FROM schedule_assignments WHERE item_id IN (?)',
		[items.map((i) => i.id)]
	);
	const personsByItem = new Map<number, number[]>();
	for (const a of assignments) {
		const list = personsByItem.get(a.item_id) ?? [];
		list.push(a.person_id);
		personsByItem.set(a.item_id, list);
	}

	return items.map((i) => ({
		id: i.id,
		dayId: i.day_id,
		time: i.time,
		title: i.title,
		locationId: i.loc_id,
		location: i.loc_id ? { id: i.loc_id, name: i.loc_name, lat: i.loc_lat, lng: i.loc_lng } : null,
		teamInfo: Boolean(i.team_info),
		sortOrder: i.sort_order,
		personIds: personsByItem.get(i.id) ?? []
	}));
}

interface ItemInput {
	dayId: number;
	time: string;
	title: string;
	locationId: number | null;
	teamInfo: boolean;
}

export async function createItem(input: ItemInput): Promise<number> {
	const pool = await db();
	const [[{ maxOrder }]] = await pool.query<RowDataPacket[]>(
		'SELECT COALESCE(MAX(sort_order), -1) + 1 AS maxOrder FROM schedule_items WHERE day_id = ?',
		[input.dayId]
	);
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO schedule_items (day_id, time, title, location_id, team_info, sort_order) VALUES (?, ?, ?, ?, ?, ?)',
		[input.dayId, input.time, input.title, input.locationId, input.teamInfo, maxOrder]
	);
	return result.insertId;
}

export async function updateItem(id: number, input: ItemInput): Promise<void> {
	const pool = await db();
	await pool.execute(
		'UPDATE schedule_items SET time = ?, title = ?, location_id = ?, team_info = ? WHERE id = ?',
		[input.time, input.title, input.locationId, input.teamInfo, id]
	);
}

export async function deleteItem(id: number): Promise<void> {
	const pool = await db();
	await pool.execute('DELETE FROM schedule_items WHERE id = ?', [id]);
}

export async function setAssignment(itemId: number, personId: number, assigned: boolean): Promise<void> {
	const pool = await db();
	if (assigned) {
		await pool.execute('INSERT IGNORE INTO schedule_assignments (item_id, person_id) VALUES (?, ?)', [
			itemId,
			personId
		]);
	} else {
		await pool.execute('DELETE FROM schedule_assignments WHERE item_id = ? AND person_id = ?', [itemId, personId]);
	}
}

// ---- Nachrichten aus dem Zeitplan bauen -------------------------------------

function formatMessageBody(items: ScheduleItem[], people: Person[], showTeamColumn: boolean): string {
	const peopleById = new Map(people.map((p) => [p.id, p.name]));
	const header = ['Uhrzeit', 'Programmpunkt', 'Ort', ...(showTeamColumn ? ['Team'] : [])];
	const rows = items.map((item) => {
		const team =
			showTeamColumn && item.teamInfo
				? `(${item.personIds.map((id) => peopleById.get(id)).filter(Boolean).join(',')})`
				: '';
		return [item.time || '-', item.title || '-', item.location?.name || '-', ...(showTeamColumn ? [team] : [])];
	});
	return [header.join('\t'), ...rows.map((r) => r.join('\t'))].join('\n');
}

export async function buildMessagesForDay(dayId: number, slackUsers: SlackUser[]): Promise<MessageEntry[]> {
	const [day, items, people] = await Promise.all([
		listDays().then((days) => days.find((d) => d.id === dayId)),
		listItems(dayId),
		listPeople()
	]);
	if (!day) throw new Error('Tag nicht gefunden.');

	const showTeamColumn = items.some((i) => i.teamInfo);

	return people.map((person) => {
		const relevantItems = items
			.filter((i) => i.personIds.includes(person.id))
			.sort((a, b) => a.sortOrder - b.sortOrder);

		const bm = bestSlackMatch(person.name, slackUsers);
		return {
			personName: person.name,
			nameSlack: bm.name,
			matchScore: bm.score,
			matchQuality: matchQuality(bm.score),
			nItems: relevantItems.length,
			include: true,
			messageHead: `Zeitplan für ${person.name}\nTag: ${day.label}`,
			messageBody: formatMessageBody(relevantItems, people, showTeamColumn)
		} satisfies MessageEntry;
	});
}

// ---- Read-only Ansicht fuer Nicht-Admins ------------------------------------

export interface PersonalScheduleDay {
	day: ScheduleDay;
	items: ScheduleItem[];
}

export async function getPersonalSchedule(personId: number): Promise<PersonalScheduleDay[]> {
	const days = await listDays();
	const perDay = await Promise.all(
		days.map(async (day) => ({
			day,
			items: (await listItems(day.id)).filter((i) => i.personIds.includes(personId))
		}))
	);
	return perDay.filter((d) => d.items.length > 0);
}
