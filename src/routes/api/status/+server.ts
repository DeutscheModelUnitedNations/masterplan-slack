import { json } from '@sveltejs/kit';
import { getStatus } from '$lib/server/slack';

export async function GET() {
	const status = await getStatus();
	return json({ status });
}
