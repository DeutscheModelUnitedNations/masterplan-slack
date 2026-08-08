import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createItem, listItems } from '$lib/server/schedule';

export async function GET({ url, locals }) {
	requireAdmin(locals);
	const dayId = Number(url.searchParams.get('dayId'));
	if (!dayId) error(400, 'dayId fehlt.');
	return json({ items: await listItems(dayId) });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const body = (await request.json()) as {
		dayId: number;
		time: string;
		title: string;
		locationId: number | null;
		teamInfo: boolean;
	};
	const id = await createItem(body);
	return json({ id });
}
