import type { MatchQuality, Person, SlackUser } from './types';

// Namens-Matching: normalisiert (Kleinschreibung, Umlaute, Diakritika, Satzzeichen),
// vergleicht token-sortiert per Jaro-Winkler und beruecksichtigt Teilmengen
// (z. B. fehlende Mittelnamen / Initialen). Portiert aus app.R.

export function normalizeName(input: string | null | undefined): string {
	if (!input) return '';
	const folded = input
		.toLowerCase()
		.replace(/ä/g, 'ae')
		.replace(/ö/g, 'oe')
		.replace(/ü/g, 'ue')
		.replace(/ß/g, 'ss')
		.normalize('NFD')
		.replace(/\p{Diacritic}/gu, '')
		.replace(/[^a-z0-9]+/g, ' ');
	return folded.trim().replace(/\s+/g, ' ');
}

function tokenSort(name: string): string {
	return name.split(' ').filter(Boolean).sort().join(' ');
}

function jaro(a: string, b: string): number {
	if (a === b) return 1;
	const lenA = a.length;
	const lenB = b.length;
	if (lenA === 0 || lenB === 0) return 0;

	const matchWindow = Math.max(0, Math.floor(Math.max(lenA, lenB) / 2) - 1);
	const aMatched = new Array(lenA).fill(false);
	const bMatched = new Array(lenB).fill(false);
	let matches = 0;

	for (let i = 0; i < lenA; i++) {
		const start = Math.max(0, i - matchWindow);
		const end = Math.min(i + matchWindow + 1, lenB);
		for (let j = start; j < end; j++) {
			if (bMatched[j] || a[i] !== b[j]) continue;
			aMatched[i] = true;
			bMatched[j] = true;
			matches++;
			break;
		}
	}
	if (matches === 0) return 0;

	let transpositions = 0;
	let k = 0;
	for (let i = 0; i < lenA; i++) {
		if (!aMatched[i]) continue;
		while (!bMatched[k]) k++;
		if (a[i] !== b[k]) transpositions++;
		k++;
	}

	return (matches / lenA + matches / lenB + (matches - transpositions / 2) / matches) / 3;
}

// stringdist::stringsim(method = "jw", p = 0.1)
function jaroWinkler(a: string, b: string, p = 0.1): number {
	const sim = jaro(a, b);
	let prefix = 0;
	for (let i = 0; i < Math.min(4, a.length, b.length); i++) {
		if (a[i] !== b[i]) break;
		prefix++;
	}
	return sim + prefix * p * (1 - sim);
}

export function nameSimilarity(a: string, b: string): number {
	const na = normalizeName(a);
	const nb = normalizeName(b);
	if (!na || !nb) return 0;
	const ta = na.split(' ').filter(Boolean);
	const tb = nb.split(' ').filter(Boolean);
	const jw = jaroWinkler(tokenSort(na), tokenSort(nb));
	const overlap = ta.filter((t) => tb.includes(t)).length;
	const setSim = ta.length && tb.length ? overlap / Math.max(ta.length, tb.length) : 0;
	return Math.max(jw, setSim);
}

export function bestMatch(name: string, candidates: string[]): { name: string | null; score: number } {
	if (candidates.length === 0) return { name: null, score: 0 };
	let best = candidates[0];
	let bestScore = nameSimilarity(name, best);
	for (const candidate of candidates.slice(1)) {
		const score = nameSimilarity(name, candidate);
		if (score > bestScore) {
			best = candidate;
			bestScore = score;
		}
	}
	return { name: best, score: bestScore };
}

export function bestSlackMatch(name: string, users: SlackUser[]): { name: string | null; score: number } {
	return bestMatch(
		name,
		users.map((u) => u.realName)
	);
}

export function matchQuality(score: number | null | undefined): MatchQuality {
	if (score == null || Number.isNaN(score)) return 'poor';
	if (score >= 0.9) return 'good';
	if (score >= 0.75) return 'weak';
	return 'poor';
}

// dmun.de-Adressen folgen dem Schema v.nachname@dmun.de (erster Buchstabe des
// Vornamens + Nachname). "v.berger" fuer "Anna Berger" ergibt z.B. "a.berger".
function expectedLocalPart(personName: string): string {
	const tokens = normalizeName(personName).split(' ').filter(Boolean);
	if (tokens.length === 0) return '';
	return `${tokens[0][0]}.${tokens[tokens.length - 1]}`;
}

// Ab dieser Aehnlichkeit wird ein geratener Treffer automatisch akzeptiert.
// Bewusst hoch angesetzt: ein falscher Login-Treffer wuerde fremde Zeitplaene
// offenlegen, deshalb lieber "kein Treffer" als "vermutlich richtig".
export const AUTO_MATCH_THRESHOLD = 0.92;

export interface PersonMatch {
	person: Person | null;
	score: number;
	// true = automatisch sicher zugeordnet (exakte E-Mail oder sehr hohe
	// Namens-Aehnlichkeit). false = kein verlaesslicher Treffer, sollte von
	// einem Admin per expliziter E-Mail am Personendatensatz geklaert werden.
	confident: boolean;
}

export function matchPersonByEmail(email: string, people: Person[]): PersonMatch {
	const normalizedEmail = email.trim().toLowerCase();
	const explicit = people.find((p) => p.email?.trim().toLowerCase() === normalizedEmail);
	if (explicit) return { person: explicit, score: 1, confident: true };

	const local = normalizedEmail.split('@')[0] ?? '';
	let best: Person | null = null;
	let bestScore = 0;
	for (const person of people) {
		const score = nameSimilarity(expectedLocalPart(person.name), local);
		if (score > bestScore) {
			bestScore = score;
			best = person;
		}
	}
	const confident = bestScore >= AUTO_MATCH_THRESHOLD;
	return { person: confident ? best : null, score: bestScore, confident };
}
