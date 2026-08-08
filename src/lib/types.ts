export type Workspace = 'MUNBW' | 'MUNBB' | 'MUN-SH' | 'DMUN';

export const WORKSPACES: Workspace[] = ['MUNBW', 'MUNBB', 'MUN-SH', 'DMUN'];

export interface SlackStatus {
	connected: boolean;
	team: string | null;
	user: string | null;
	age: string | null;
	workspace: Workspace;
}

export interface SlackUser {
	id: string;
	realName: string;
}

export type MatchQuality = 'good' | 'weak' | 'poor';

export interface MessageEntry {
	personName: string;
	nameSlack: string | null;
	matchScore: number;
	matchQuality: MatchQuality;
	nItems: number;
	include: boolean;
	messageHead: string;
	messageBody: string;
}
