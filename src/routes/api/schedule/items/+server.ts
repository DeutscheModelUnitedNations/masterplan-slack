import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createItem, createItemOnDays, listItems } from '$lib/server/schedule';

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
		endTime: string | null;
		title: string;
		locationId: number | null;
		teamInfo: boolean;
		// Zusaetzliche Tage, auf denen derselbe Programmpunkt wiederkehrend angelegt wird.
		repeatOnDayIds?: number[];
	};
	const { repeatOnDayIds, ...input } = body;
	const id = await createItem(input);
	const extraDayIds = (repeatOnDayIds ?? []).filter((dayId) => dayId !== body.dayId);
	const repeatedIds = extraDayIds.length > 0 ? await createItemOnDays(extraDayIds, input) : [];
	return json({ id, repeatedIds });
}
