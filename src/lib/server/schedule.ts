import type { RowDataPacket } from 'mysql2';
import { bestSlackMatch, matchPersonByEmail, matchQuality, type PersonMatch } from '$lib/matching';
import type { Conference, Group, Location, MessageEntry, Person, ScheduleDay, ScheduleItem, SlackUser } from '$lib/types';
import { db } from './db';

// ---- Konferenzen -------------------------------------------------------------

export async function listConferences(): Promise<Conference[]> {
	const pool = await db();
	const [rows] = await pool.query<RowDataPacket[]>('SELECT id, name FROM conferences ORDER BY id DESC');
	return rows.map((r) => ({ id: r.id, name: r.name }));
}

export async function createConference(name: string): Promise<Conference> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>('INSERT INTO conferences (name) VALUES (?)', [
		name
	]);
	return { id: result.insertId, name };
}

export async function renameConference(id: number, name: string): Promise<void> {
	const pool = await db();
	await pool.execute('UPDATE conferences SET name = ? WHERE id = ?', [name, id]);
}

export async function deleteConference(id: number): Promise<void> {
	const pool = await db();
	await pool.execute('DELETE FROM conferences WHERE id = ?', [id]);
}

// ---- Personen -------------------------------------------------------------
// conferenceId weglassen = ueber alle Konferenzen suchen (nur fuers Login-Matching gedacht).

export async function listPeople(conferenceId?: number): Promise<Person[]> {
	const pool = await db();
	const [rows] = conferenceId
		? await pool.query<RowDataPacket[]>(
				'SELECT id, conference_id, name, email FROM people WHERE conference_id = ? ORDER BY name',
				[conferenceId]
			)
		: await pool.query<RowDataPacket[]>('SELECT id, conference_id, name, email FROM people ORDER BY name');
	return rows.map((r) => ({ id: r.id, conferenceId: r.conference_id, name: r.name, email: r.email }));
}

export async function createPerson(conferenceId: number, name: string, email: string | null): Promise<Person> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO people (conference_id, name, email) VALUES (?, ?, ?)',
		[conferenceId, name, email || null]
	);
	return { id: result.insertId, conferenceId, name, email };
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
	const people = await listPeople(); // ueber alle Konferenzen - Login weiss ja noch nicht, welche gemeint ist
	return matchPersonByEmail(email, people);
}

// ---- Locations (fuer die OSM-Karte) -----------------------------------------

export async function listLocations(conferenceId?: number): Promise<Location[]> {
	const pool = await db();
	const [rows] = conferenceId
		? await pool.query<RowDataPacket[]>(
				'SELECT id, conference_id, name, lat, lng FROM locations WHERE conference_id = ? ORDER BY name',
				[conferenceId]
			)
		: await pool.query<RowDataPacket[]>('SELECT id, conference_id, name, lat, lng FROM locations ORDER BY name');
	return rows.map((r) => ({ id: r.id, conferenceId: r.conference_id, name: r.name, lat: r.lat, lng: r.lng }));
}

export async function createLocation(
	conferenceId: number,
	name: string,
	lat: number | null,
	lng: number | null
): Promise<Location> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO locations (conference_id, name, lat, lng) VALUES (?, ?, ?, ?)',
		[conferenceId, name, lat, lng]
	);
	return { id: result.insertId, conferenceId, name, lat, lng };
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

export async function listGroups(conferenceId?: number): Promise<Group[]> {
	const pool = await db();
	const [groups] = conferenceId
		? await pool.query<RowDataPacket[]>(
				'SELECT id, conference_id, name FROM person_groups WHERE conference_id = ? ORDER BY name',
				[conferenceId]
			)
		: await pool.query<RowDataPacket[]>('SELECT id, conference_id, name FROM person_groups ORDER BY name');
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
	return groups.map((g) => ({ id: g.id, conferenceId: g.conference_id, name: g.name, personIds: byGroup.get(g.id) ?? [] }));
}

export async function createGroup(conferenceId: number, name: string): Promise<Group> {
	const pool = await db();
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO person_groups (conference_id, name) VALUES (?, ?)',
		[conferenceId, name]
	);
	return { id: result.insertId, conferenceId, name, personIds: [] };
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

export async function listDays(conferenceId?: number): Promise<ScheduleDay[]> {
	const pool = await db();
	const [rows] = conferenceId
		? await pool.query<RowDataPacket[]>(
				'SELECT id, conference_id, label, sort_order FROM schedule_days WHERE conference_id = ? ORDER BY sort_order, id',
				[conferenceId]
			)
		: await pool.query<RowDataPacket[]>(
				'SELECT id, conference_id, label, sort_order FROM schedule_days ORDER BY sort_order, id'
			);
	return rows.map((r) => ({ id: r.id, conferenceId: r.conference_id, label: r.label, sortOrder: r.sort_order }));
}

export async function createDay(conferenceId: number, label: string): Promise<ScheduleDay> {
	const pool = await db();
	const [[{ maxOrder }]] = await pool.query<RowDataPacket[]>(
		'SELECT COALESCE(MAX(sort_order), -1) + 1 AS maxOrder FROM schedule_days WHERE conference_id = ?',
		[conferenceId]
	);
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO schedule_days (conference_id, label, sort_order) VALUES (?, ?, ?)',
		[conferenceId, label, maxOrder]
	);
	return { id: result.insertId, conferenceId, label, sortOrder: maxOrder };
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
		        l.id AS loc_id, l.conference_id AS loc_conference_id, l.name AS loc_name, l.lat AS loc_lat, l.lng AS loc_lng
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
		location: i.loc_id
			? { id: i.loc_id, conferenceId: i.loc_conference_id, name: i.loc_name, lat: i.loc_lat, lng: i.loc_lng }
			: null,
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
	const [day, items] = await Promise.all([listDays().then((days) => days.find((d) => d.id === dayId)), listItems(dayId)]);
	if (!day) throw new Error('Tag nicht gefunden.');

	const people = await listPeople(day.conferenceId);
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

export async function getPersonalSchedule(personId: number, conferenceId: number): Promise<PersonalScheduleDay[]> {
	const days = await listDays(conferenceId);
	const perDay = await Promise.all(
		days.map(async (day) => ({
			day,
			items: (await listItems(day.id)).filter((i) => i.personIds.includes(personId))
		}))
	);
	return perDay.filter((d) => d.items.length > 0);
}
