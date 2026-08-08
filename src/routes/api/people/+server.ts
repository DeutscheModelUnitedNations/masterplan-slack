import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createPerson, listPeople } from '$lib/server/schedule';

export async function GET({ locals }) {
	requireAdmin(locals);
	return json({ people: await listPeople() });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { name, email } = (await request.json()) as { name: string; email: string | null };
	const person = await createPerson(name.trim(), email?.trim() || null);
	return json({ person });
}
