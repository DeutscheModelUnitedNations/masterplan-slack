import { json } from '@sveltejs/kit';
import { requireAdmin } from '$lib/server/auth';
import { searchAddress } from '$lib/server/geocode';

export async function GET({ url, locals }) {
	requireAdmin(locals);
	const q = url.searchParams.get('q')?.trim();
	if (!q || q.length < 3) return json({ results: [] });

	try {
		return json({ results: await searchAddress(q) });
	} catch {
		return json({ results: [] });
	}
}
