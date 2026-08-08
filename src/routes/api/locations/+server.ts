import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createLocation, listLocations } from '$lib/server/schedule';

export async function GET({ locals }) {
	requireAdmin(locals);
	return json({ locations: await listLocations() });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { name, lat, lng } = (await request.json()) as { name: string; lat: number | null; lng: number | null };
	const location = await createLocation(name.trim(), lat, lng);
	return json({ location });
}
