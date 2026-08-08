import { env } from '$env/dynamic/private';
import type { Workspace } from '$lib/types';

export interface WorkspaceConfig {
	workspace: Workspace;
	slackToken: string;
	testChannel: string;
}

// Slack-Umgebungsvariablen sind pro Workspace unter einem eigenen Suffix
// hinterlegt, siehe docker-compose.yaml. "MUN-SH" nutzt "MUNSH" als Suffix.
const ENV_SUFFIX: Record<Workspace, string> = {
	MUNBW: 'MUNBW',
	MUNBB: 'MUNBB',
	'MUN-SH': 'MUNSH',
	DMUN: 'DMUN'
};

export function workspaceConfig(ws: Workspace): WorkspaceConfig {
	const suffix = ENV_SUFFIX[ws];
	return {
		workspace: ws,
		slackToken: env[`SLACK_TOKEN_${suffix}`] ?? '',
		testChannel: env[`TEST_CHANNEL_${suffix}`] ?? ''
	};
}
