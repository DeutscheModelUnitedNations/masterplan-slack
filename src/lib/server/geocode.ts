// Adress-Suche ueber Nominatim, den kostenlosen Geocoder von OpenStreetMap.
// Kein API-Key noetig, nur ein aussagekraeftiger User-Agent (siehe Nutzungsrichtlinien:
// https://operations.osmfoundation.org/policies/nominatim/).

export interface GeocodeResult {
	name: string;
	lat: number;
	lng: number;
}

export async function searchAddress(query: string): Promise<GeocodeResult[]> {
	const url = new URL('https://nominatim.openstreetmap.org/search');
	url.searchParams.set('q', query);
	url.searchParams.set('format', 'jsonv2');
	url.searchParams.set('limit', '5');

	const res = await fetch(url, {
		headers: { 'User-Agent': 'dmun-tmk-bot/1.0 (github.com/DeutscheModelUnitedNations)' }
	});
	if (!res.ok) throw new Error('Adress-Suche fehlgeschlagen.');

	const data = (await res.json()) as { display_name: string; lat: string; lon: string }[];
	return data.map((r) => ({ name: r.display_name, lat: Number(r.lat), lng: Number(r.lon) }));
}
