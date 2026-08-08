import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { setAssignment } from '$lib/server/schedule';

export async function POST({ request, params, locals }) {
	requireAdmin(locals);
	const { personId, assigned } = (await request.json()) as { personId: number; assigned: boolean };
	await setAssignment(Number(params.id), personId, assigned);
	return json({ ok: true });
}
