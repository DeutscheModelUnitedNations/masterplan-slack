import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { deleteItem, updateItem } from '$lib/server/schedule';

export async function PATCH({ request, params, locals }) {
	requireAdmin(locals);
	const body = (await request.json()) as {
		dayId: number;
		time: string;
		title: string;
		location: string;
		teamInfo: boolean;
	};
	await updateItem(Number(params.id), body);
	return json({ ok: true });
}

export async function DELETE({ params, locals }) {
	requireAdmin(locals);
	await deleteItem(Number(params.id));
	return json({ ok: true });
}
