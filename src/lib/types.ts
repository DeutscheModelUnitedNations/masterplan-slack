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

export interface Conference {
	id: number;
	name: string;
}

export interface Person {
	id: number;
	conferenceId: number;
	name: string;
	// Explizit hinterlegte Adresse hat beim Login-Matching Vorrang vor dem
	// geratenen Schema v.nachname@dmun.de - siehe matchPersonByEmail().
	email: string | null;
}

export interface ScheduleDay {
	id: number;
	conferenceId: number;
	label: string;
	sortOrder: number;
}

export interface Location {
	id: number;
	conferenceId: number;
	name: string;
	lat: number | null;
	lng: number | null;
}

export interface ScheduleItem {
	id: number;
	dayId: number;
	time: string;
	endTime: string | null;
	title: string;
	locationId: number | null;
	location: Location | null;
	teamInfo: boolean;
	sortOrder: number;
	personIds: number[];
}

export interface Group {
	id: number;
	conferenceId: number;
	name: string;
	personIds: number[];
}
