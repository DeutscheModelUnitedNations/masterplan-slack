import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { deleteConference, renameConference } from '$lib/server/schedule';

export async function PATCH({ request, params, locals }) {
	requireAdmin(locals);
	const { name } = (await request.json()) as { name: string };
	await renameConference(Number(params.id), name.trim());
	return json({ ok: true });
}

export async function DELETE({ params, locals }) {
	requireAdmin(locals);
	await deleteConference(Number(params.id));
	return json({ ok: true });
}
