import { json } from '@sveltejs/kit';
import { findPersonForEmail, getPersonalSchedule, listConferences } from '$lib/server/schedule';

// Kein requireAdmin() hier bewusst - jeder eingeloggte Nutzer darf seinen
// eigenen Zeitplan sehen, aber wirklich nur seinen eigenen (siehe matchPersonByEmail).
export async function GET({ locals }) {
	if (!locals.email) return json({ match: null, days: [] });

	const match = await findPersonForEmail(locals.email);
	if (!match.person) return json({ match: null, days: [] });

	const [days, conferences] = await Promise.all([
		getPersonalSchedule(match.person.id, match.person.conferenceId),
		listConferences()
	]);
	const conferenceName = conferences.find((c) => c.id === match.person!.conferenceId)?.name ?? '';

	return json({ match: { name: match.person.name, confident: match.confident, conferenceName }, days });
}
