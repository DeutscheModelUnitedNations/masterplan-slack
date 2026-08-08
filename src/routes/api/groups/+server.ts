import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createGroup, listGroups } from '$lib/server/schedule';

export async function GET({ url, locals }) {
	requireAdmin(locals);
	const conferenceId = Number(url.searchParams.get('conferenceId'));
	if (!conferenceId) error(400, 'conferenceId fehlt.');
	return json({ groups: await listGroups(conferenceId) });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { conferenceId, name } = (await request.json()) as { conferenceId: number; name: string };
	const group = await createGroup(conferenceId, name.trim());
	return json({ group });
}
