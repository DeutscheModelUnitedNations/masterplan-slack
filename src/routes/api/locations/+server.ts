import { error, json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { createLocation, listLocations } from '$lib/server/schedule';

export async function GET({ url, locals }) {
	requireAdmin(locals);
	const conferenceId = Number(url.searchParams.get('conferenceId'));
	if (!conferenceId) error(400, 'conferenceId fehlt.');
	return json({ locations: await listLocations(conferenceId) });
}

export async function POST({ request, locals }) {
	requireAdmin(locals);
	const { conferenceId, name, lat, lng } = (await request.json()) as {
		conferenceId: number;
		name: string;
		lat: number | null;
		lng: number | null;
	};
	const location = await createLocation(conferenceId, name.trim(), lat, lng);
	return json({ location });
}
