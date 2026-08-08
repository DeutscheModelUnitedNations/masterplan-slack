import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createDay, listDays } from '$lib/server/schedule';

export async function GET({ url, locals }) {
	requireAdmin(locals);
	const conferenceId = Number(url.searchParams.get('conferenceId'));
	if (!conferenceId) error(400, 'conferenceId fehlt.');
	return json({ days: await listDays(conferenceId) });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { conferenceId, label } = (await request.json()) as { conferenceId: number; label: string };
	const day = await createDay(conferenceId, label.trim());
	return json({ day });
}
