import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createPerson, listPeople } from '$lib/server/schedule';

export async function GET({ url, locals }) {
	requireAdmin(locals);
	const conferenceId = Number(url.searchParams.get('conferenceId'));
	if (!conferenceId) error(400, 'conferenceId fehlt.');
	return json({ people: await listPeople(conferenceId) });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { conferenceId, name, email } = (await request.json()) as {
		conferenceId: number;
		name: string;
		email: string | null;
	};
	const person = await createPerson(conferenceId, name.trim(), email?.trim() || null);
	return json({ person });
}
