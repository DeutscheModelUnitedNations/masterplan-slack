import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createConference, listConferences } from '$lib/server/schedule';

export async function GET({ locals }) {
	requireAdmin(locals);
	return json({ conferences: await listConferences() });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { name } = (await request.json()) as { name: string };
	const conference = await createConference(name.trim());
	return json({ conference });
}
