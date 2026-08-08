import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createDay, listDays } from '$lib/server/schedule';

export async function GET({ locals }) {
	requireAdmin(locals);
	return json({ days: await listDays() });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { label } = (await request.json()) as { label: string };
	const day = await createDay(label.trim());
	return json({ day });
}
