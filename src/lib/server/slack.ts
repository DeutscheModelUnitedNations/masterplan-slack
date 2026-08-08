import { WebClient } from '@slack/web-api';
import type { SlackStatus, SlackUser, Workspace } from '$lib/types';
import { workspaceConfig, type WorkspaceConfig } from './workspaces';

// Ein einzelner, geteilter Slack-Login fuer den ganzen Prozess - wie im
// Original ein bewusst einfacher globaler Zustand statt Session-Verwaltung.
let currentWorkspace: Workspace = 'MUNBW';
let currentConfig: WorkspaceConfig = workspaceConfig(currentWorkspace);
let client = new WebClient(currentConfig.slackToken);

let cachedUsers: SlackUser[] = [];
let lastRefreshed: string | null = null;
let initialLoad: Promise<void> | null = null;

function formatAge(date: Date) {
	return new Intl.DateTimeFormat('de-DE', {
		weekday: 'long',
		day: '2-digit',
		month: 'long',
		year: 'numeric',
		hour: '2-digit',
		minute: '2-digit',
		second: '2-digit'
	}).format(date);
}

async function refreshUsers() {
	const result = await client.users.list({ limit: 1000 });
	cachedUsers = (result.members ?? [])
		.filter((m) => !m.deleted && m.real_name)
		.map((m) => ({ id: m.id!, realName: m.real_name! }));
	lastRefreshed = formatAge(new Date());
}

function ensureInitialLoad() {
	if (!initialLoad) initialLoad = refreshUsers().catch(() => {});
	return initialLoad;
}

export async function getStatus(): Promise<SlackStatus> {
	await ensureInitialLoad();
	try {
		const auth = await client.auth.test();
		return {
			connected: true,
			team: (auth.team as string) ?? null,
			user: (auth.user as string) ?? null,
			age: lastRefreshed,
			workspace: currentWorkspace
		};
	} catch {
		return { connected: false, team: null, user: null, age: lastRefreshed, workspace: currentWorkspace };
	}
}

export async function switchWorkspace(ws: Workspace): Promise<SlackStatus> {
	currentWorkspace = ws;
	currentConfig = workspaceConfig(ws);
	client = new WebClient(currentConfig.slackToken);
	initialLoad = null;
	await refreshUsers().catch(() => {});
	return getStatus();
}

export async function refreshSlackData(): Promise<SlackStatus> {
	await refreshUsers().catch(() => {});
	return getStatus();
}

export async function getUsers(): Promise<SlackUser[]> {
	await ensureInitialLoad();
	return cachedUsers;
}

export function getWorkspaceConfig() {
	return currentConfig;
}

interface SendResult {
	recipient: string;
	ok: boolean;
}

export async function sendMessages(
	entries: { recipient: string; text: string }[],
	test: boolean
): Promise<SendResult[]> {
	const results: SendResult[] = [];
	for (const entry of entries) {
		const channel = test ? currentConfig.testChannel : cachedUsers.find((u) => u.realName === entry.recipient)?.id;
		if (!channel) {
			results.push({ recipient: entry.recipient, ok: false });
			continue;
		}
		try {
			const res = await client.chat.postMessage({ channel, text: entry.text });
			results.push({ recipient: entry.recipient, ok: Boolean(res.ok) });
		} catch {
			results.push({ recipient: entry.recipient, ok: false });
		}
	}
	return results;
}

// Abschlussmeldung nach dem Versand - Fehlerhinweis oder tageszeitabhaengiger
// Gluckwunsch, in lokaler Zeit (Europe/Berlin), wie im Original.
export function summarizeSend(results: SendResult[]): string {
	const sent = results.filter((r) => r.ok).length;
	const errors = results.filter((r) => !r.ok).map((r) => r.recipient);
	const time = new Intl.DateTimeFormat('de-DE', {
		hour: '2-digit',
		minute: '2-digit',
		second: '2-digit',
		hour12: false,
		timeZone: 'Europe/Berlin'
	}).format(new Date());

	if (errors.length > 0) {
		return `Es gab gerade ${time} einen Fehler beim Versenden von ${errors.length} Nachricht(en) (${errors.join(', ')}). Die übrigen ${sent} wurden versendet.`;
	}
	if (time > '23:00:00' || time < '06:00:00') {
		return `Herzlichen Glückwunsch, es wurden nun (${time}) ${sent} Nachrichten verschickt. Jetzt darfst du beruhigt ins Bett gehen :)`;
	}
	if (time > '20:00:00') {
		return `Herzlichen Glückwunsch, es wurden nun (${time}) ${sent} Nachrichten verschickt. Jetzt hast du dir ein Bier verdient.`;
	}
	return `Herzlichen Glückwunsch, es wurden nun (${time}) ${sent} Nachrichten verschickt.`;
}
