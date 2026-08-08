import { json } from '@sveltejs/kit';
import { findPersonForEmail, getPersonalSchedule } from '$lib/server/schedule';

// Kein requireAdmin() hier bewusst - jeder eingeloggte Nutzer darf seinen
// eigenen Zeitplan sehen, aber wirklich nur seinen eigenen (siehe matchPersonByEmail).
export async function GET({ locals }) {
	if (!locals.email) return json({ match: null, days: [] });

	const match = await findPersonForEmail(locals.email);
	if (!match.person) return json({ match: null, days: [] });

	const days = await getPersonalSchedule(match.person.id);
	return json({ match: { name: match.person.name, confident: match.confident }, days });
}
