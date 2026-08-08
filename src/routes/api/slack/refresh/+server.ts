import { json } from '@sveltejs/kit';
import { refreshSlackData } from '$lib/server/slack';

export async function POST() {
	const status = await refreshSlackData();
	return json({ status });
}
