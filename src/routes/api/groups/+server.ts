import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createGroup, listGroups } from '$lib/server/schedule';

export async function GET({ locals }) {
	requireAdmin(locals);
	return json({ groups: await listGroups() });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { name } = (await request.json()) as { name: string };
	const group = await createGroup(name.trim());
	return json({ group });
}
