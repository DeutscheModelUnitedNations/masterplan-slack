import type { RowDataPacket } from 'mysql2';
import { bestSlackMatch, matchPersonByEmail, matchQuality, type PersonMatch } from '$lib/matching';
import type { MessageEntry, Person, ScheduleDay, ScheduleItem, SlackUser } from '$lib/types';
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
		'SELECT id, day_id, time, title, location, team_info, sort_order FROM schedule_items WHERE day_id = ? ORDER BY sort_order, id',
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
		location: i.location,
		teamInfo: Boolean(i.team_info),
		sortOrder: i.sort_order,
		personIds: personsByItem.get(i.id) ?? []
	}));
}

interface ItemInput {
	dayId: number;
	time: string;
	title: string;
	location: string;
	teamInfo: boolean;
}

export async function createItem(input: ItemInput): Promise<number> {
	const pool = await db();
	const [[{ maxOrder }]] = await pool.query<RowDataPacket[]>(
		'SELECT COALESCE(MAX(sort_order), -1) + 1 AS maxOrder FROM schedule_items WHERE day_id = ?',
		[input.dayId]
	);
	const [result] = await pool.execute<import('mysql2').ResultSetHeader>(
		'INSERT INTO schedule_items (day_id, time, title, location, team_info, sort_order) VALUES (?, ?, ?, ?, ?, ?)',
		[input.dayId, input.time, input.title, input.location, input.teamInfo, maxOrder]
	);
	return result.insertId;
}

export async function updateItem(id: number, input: ItemInput): Promise<void> {
	const pool = await db();
	await pool.execute('UPDATE schedule_items SET time = ?, title = ?, location = ?, team_info = ? WHERE id = ?', [
		input.time,
		input.title,
		input.location,
		input.teamInfo,
		id
	]);
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
		return [item.time || '-', item.title || '-', item.location || '-', ...(showTeamColumn ? [team] : [])];
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
