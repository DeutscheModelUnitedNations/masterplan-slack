import { json } from '@sveltejs/kit';
import { getUsers } from '$lib/server/slack';

export async function GET() {
	const users = await getUsers();
	return json({ users });
}
