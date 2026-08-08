import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { getUsers } from '$lib/server/slack';
import { buildMessagesForDay } from '$lib/server/schedule';

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { dayId } = (await request.json()) as { dayId: number };
	if (!dayId) error(400, 'dayId fehlt.');

	const users = await getUsers();
	const messages = await buildMessagesForDay(dayId, users);
	return json({ messages });
}
