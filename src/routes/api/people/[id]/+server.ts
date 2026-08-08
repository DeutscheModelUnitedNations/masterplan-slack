import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { deletePerson, updatePerson } from '$lib/server/schedule';

export async function PATCH({ request, params, locals }) {
	requireAdmin(locals);
	const { name, email } = (await request.json()) as { name: string; email: string | null };
	await updatePerson(Number(params.id), name.trim(), email?.trim() || null);
	return json({ ok: true });
}

export async function DELETE({ params, locals }) {
	requireAdmin(locals);
	await deletePerson(Number(params.id));
	return json({ ok: true });
}
