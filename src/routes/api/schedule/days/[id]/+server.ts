import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { deleteDay } from '$lib/server/schedule';

export async function DELETE({ params, locals }) {
	requireAdmin(locals);
	await deleteDay(Number(params.id));
	return json({ ok: true });
}
