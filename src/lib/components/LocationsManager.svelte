<script lang="ts">
	import type { Location } from '$lib/types';
	import LocationMap from './LocationMap.svelte';

	let {
		locations,
		conferenceId,
		reload
	}: { locations: Location[]; conferenceId: number | null; reload: () => void } = $props();

	let newName = $state('');
	let newLat = $state<number | null>(null);
	let newLng = $state<number | null>(null);
	let saving = $state(false);
	let showPicker = $state(false);

	interface GeocodeResult {
		name: string;
		lat: number;
		lng: number;
	}
	let addressQuery = $state('');
	let searchResults = $state<GeocodeResult[]>([]);
	let searching = $state(false);

	async function searchAddress() {
		if (addressQuery.trim().length < 3) return;
		searching = true;
		try {
			const res = await fetch(`/api/geocode?q=${encodeURIComponent(addressQuery.trim())}`);
			const data = await res.json();
			searchResults = data.results;
		} finally {
			searching = false;
		}
	}

	function pickResult(r: GeocodeResult) {
		newLat = r.lat;
		newLng = r.lng;
		if (!newName.trim()) newName = r.name.split(',')[0];
		searchResults = [];
		addressQuery = '';
		showPicker = true;
	}

	async function addLocation() {
		if (!newName.trim() || !conferenceId) return;
		saving = true;
		await fetch('/api/locations', {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ conferenceId, name: newName.trim(), lat: newLat, lng: newLng })
		});
		newName = '';
		newLat = null;
		newLng = null;
		showPicker = false;
		saving = false;
		reload();
	}

	async function removeLocation(id: number) {
		if (!confirm('Location wirklich löschen? Programmpunkte verlieren dann ihren Ort.')) return;
		await fetch(`/api/locations/${id}`, { method: 'DELETE' });
		reload();
	}
</script>

<div class="card bg-base-100 shadow-sm border border-base-300">
	<div class="card-body gap-3">
		<h2 class="card-title text-base">Locations</h2>

		<div class="flex flex-col gap-2 max-h-56 overflow-auto">
			{#each locations as loc (loc.id)}
				<div class="flex items-center gap-2 text-sm">
					<span class="flex-1 truncate">{loc.name}</span>
					<span class="text-xs text-base-content/50">
						{loc.lat != null && loc.lng != null ? `${loc.lat.toFixed(4)}, ${loc.lng.toFixed(4)}` : 'keine Koordinaten'}
					</span>
					<button
						class="btn btn-ghost btn-xs text-error"
						onclick={() => removeLocation(loc.id)}
						aria-label="Löschen"
					>
						<i class="fa-solid fa-trash"></i>
					</button>
				</div>
			{:else}
				<p class="text-sm text-base-content/60">
					{conferenceId ? 'Noch keine Locations angelegt.' : 'Erst eine Konferenz auswählen.'}
				</p>
			{/each}
		</div>

		<fieldset class="fieldset">
			<legend class="fieldset-legend">Name</legend>
			<input
				class="input input-bordered input-sm w-full"
				placeholder="z.B. Plenarsaal"
				bind:value={newName}
				disabled={!conferenceId}
			/>
		</fieldset>

		<fieldset class="fieldset">
			<legend class="fieldset-legend">Adresse suchen (OpenStreetMap)</legend>
			<div class="flex gap-2">
				<input
					class="input input-bordered input-sm flex-1"
					placeholder="z.B. Rathausplatz 1, Stuttgart"
					bind:value={addressQuery}
					disabled={!conferenceId}
					onkeydown={(e) => e.key === 'Enter' && searchAddress()}
				/>
				<button
					class="btn btn-outline btn-sm"
					onclick={searchAddress}
					disabled={!conferenceId || searching || addressQuery.trim().length < 3}
					aria-label="Adresse suchen"
				>
					{#if searching}
						<span class="loading loading-spinner loading-xs"></span>
					{:else}
						<i class="fa-solid fa-magnifying-glass"></i>
					{/if}
				</button>
			</div>
		</fieldset>

		{#if searchResults.length > 0}
			<div class="flex flex-col gap-1 border border-base-300 rounded-lg p-2 max-h-40 overflow-auto">
				{#each searchResults as r (r.name)}
					<button
						type="button"
						class="text-left text-xs hover:bg-base-200 rounded p-1 truncate"
						onclick={() => pickResult(r)}
					>
						{r.name}
					</button>
				{/each}
			</div>
		{/if}

		<div class="flex items-center justify-between">
			<button
				class="btn btn-ghost btn-xs"
				onclick={() => (showPicker = !showPicker)}
				disabled={!conferenceId}
			>
				<i class="fa-solid fa-map-location-dot"></i>
				{showPicker ? 'Karte ausblenden' : 'Koordinate stattdessen auf Karte setzen'}
			</button>
			<button
				class="btn btn-primary btn-sm"
				onclick={addLocation}
				disabled={saving || !newName.trim() || !conferenceId}
				aria-label="Location hinzufügen"
			>
				<i class="fa-solid fa-plus"></i>
			</button>
		</div>

		{#if showPicker}
			<p class="text-xs text-base-content/60">Auf die Karte klicken, um die Koordinate zu setzen.</p>
			<LocationMap lat={newLat} lng={newLng} editable onpick={(lat, lng) => ((newLat = lat), (newLng = lng))} />
			{#if newLat != null && newLng != null}
				<p class="text-xs text-base-content/60">{newLat.toFixed(5)}, {newLng.toFixed(5)}</p>
			{/if}
		{/if}
	</div>
</div>
