import { error, json } from '@sveltejs/kit';
import { switchWorkspace } from '$lib/server/slack';
import { WORKSPACES, type Workspace } from '$lib/types';

export async function POST({ request }) {
	const { workspace } = (await request.json()) as { workspace: Workspace };
	if (!WORKSPACES.includes(workspace)) error(400, 'Unbekannter Workspace');

	const status = await switchWorkspace(workspace);
	return json({ status });
}
