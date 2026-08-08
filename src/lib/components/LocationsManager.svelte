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

		<div class="flex gap-2">
			<input
				class="input input-bordered input-sm flex-1"
				placeholder="Name, z.B. Plenarsaal"
				bind:value={newName}
				disabled={!conferenceId}
			/>
			<button
				class="btn btn-outline btn-sm"
				onclick={() => (showPicker = !showPicker)}
				aria-label="Karte zum Setzen der Koordinate anzeigen"
				disabled={!conferenceId}
			>
				<i class="fa-solid fa-map-location-dot"></i>
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
