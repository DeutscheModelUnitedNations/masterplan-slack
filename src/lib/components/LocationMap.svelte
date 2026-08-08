<script lang="ts">
	import 'leaflet/dist/leaflet.css';
	import { onDestroy, onMount } from 'svelte';

	let {
		lat,
		lng,
		editable = false,
		onpick,
		height = '220px'
	}: {
		lat: number | null;
		lng: number | null;
		editable?: boolean;
		onpick?: (lat: number, lng: number) => void;
		height?: string;
	} = $props();

	// Frankfurt als neutraler Default-Mittelpunkt, falls noch keine Koordinate gesetzt ist.
	const FALLBACK: [number, number] = [50.1109, 8.6821];

	let container: HTMLDivElement;
	let map: import('leaflet').Map | undefined;
	let marker: import('leaflet').Marker | undefined;

	onMount(async () => {
		const L = (await import('leaflet')).default;

		// Leaflets Standard-Icon-Pfade gehen unter Vite kaputt - Bilder explizit verlinken.
		const iconRetina = (await import('leaflet/dist/images/marker-icon-2x.png')).default;
		const icon = (await import('leaflet/dist/images/marker-icon.png')).default;
		const shadow = (await import('leaflet/dist/images/marker-shadow.png')).default;
		L.Icon.Default.mergeOptions({ iconRetinaUrl: iconRetina, iconUrl: icon, shadowUrl: shadow });

		const start: [number, number] = lat != null && lng != null ? [lat, lng] : FALLBACK;
		map = L.map(container).setView(start, lat != null && lng != null ? 15 : 6);
		L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
			attribution: '&copy; OpenStreetMap contributors',
			maxZoom: 19
		}).addTo(map);

		if (lat != null && lng != null) {
			marker = L.marker(start).addTo(map);
		}

		if (editable) {
			map.on('click', (e: import('leaflet').LeafletMouseEvent) => {
				if (!map) return;
				if (marker) marker.setLatLng(e.latlng);
				else marker = L.marker(e.latlng).addTo(map);
				onpick?.(e.latlng.lat, e.latlng.lng);
			});
		}
	});

	onDestroy(() => {
		map?.remove();
	});

	$effect(() => {
		if (!map) return;
		if (lat != null && lng != null) {
			const pos: [number, number] = [lat, lng];
			if (marker) marker.setLatLng(pos);
			map.panTo(pos);
		}
	});
</script>

<div bind:this={container} style="height: {height}; border-radius: 0.5rem;"></div>
