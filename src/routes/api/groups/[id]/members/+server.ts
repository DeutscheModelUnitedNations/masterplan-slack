import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { setGroupMember } from '$lib/server/schedule';

export async function POST({ request, params, locals }) {
	requireAdmin(locals);
	const { personId, member } = (await request.json()) as { personId: number; member: boolean };
	await setGroupMember(Number(params.id), personId, member);
	return json({ ok: true });
}
