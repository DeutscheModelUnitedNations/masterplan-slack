import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { deleteLocation, updateLocation } from '$lib/server/schedule';

export async function PATCH({ request, params, locals }) {
	requireAdmin(locals);
	const { name, lat, lng } = (await request.json()) as { name: string; lat: number | null; lng: number | null };
	await updateLocation(Number(params.id), name.trim(), lat, lng);
	return json({ ok: true });
}

export async function DELETE({ params, locals }) {
	requireAdmin(locals);
	await deleteLocation(Number(params.id));
	return json({ ok: true });
}
