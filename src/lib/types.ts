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

export interface Person {
	id: number;
	name: string;
	// Explizit hinterlegte Adresse hat beim Login-Matching Vorrang vor dem
	// geratenen Schema v.nachname@dmun.de - siehe matchPersonByEmail().
	email: string | null;
}

export interface ScheduleDay {
	id: number;
	label: string;
	sortOrder: number;
}

export interface ScheduleItem {
	id: number;
	dayId: number;
	time: string;
	title: string;
	location: string;
	teamInfo: boolean;
	sortOrder: number;
	personIds: number[];
}
