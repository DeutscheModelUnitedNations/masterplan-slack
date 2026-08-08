<script lang="ts">
	import type { Conference } from '$lib/types';

	let {
		conferences,
		selectedId = $bindable(null),
		reload
	}: { conferences: Conference[]; selectedId: number | null; reload: () => Promise<void> } = $props();

	let newName = $state('');
	let saving = $state(false);

	async function addConference() {
		if (!newName.trim()) return;
		saving = true;
		const res = await fetch('/api/conferences', {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ name: newName.trim() })
		});
		const data = await res.json();
		newName = '';
		saving = false;
		await reload();
		selectedId = data.conference.id;
	}

	async function removeConference() {
		if (selectedId == null) return;
		if (!confirm('Konferenz inkl. aller Personen, Tage, Locations und Gruppen wirklich löschen?')) return;
		await fetch(`/api/conferences/${selectedId}`, { method: 'DELETE' });
		selectedId = null;
		await reload();
	}
</script>

<div class="card bg-base-100 shadow-sm border border-base-300">
	<div class="card-body gap-3">
		<h2 class="card-title text-base gap-2">
			<span
				class="w-6 h-6 rounded-full bg-primary text-primary-content text-xs font-bold flex items-center justify-center shrink-0"
				>1</span
			>
			Konferenz
		</h2>

		{#if conferences.length === 0}
			<p class="text-sm text-base-content/60">Noch keine Konferenz angelegt.</p>
		{:else}
			<div class="flex items-center gap-2">
				<select class="select select-bordered select-sm flex-1" bind:value={selectedId}>
					{#each conferences as c (c.id)}
						<option value={c.id}>{c.name}</option>
					{/each}
				</select>
				<button class="btn btn-ghost btn-xs text-error" onclick={removeConference} aria-label="Konferenz löschen">
					<i class="fa-solid fa-trash"></i>
				</button>
			</div>
		{/if}

		<div class="flex gap-2">
			<input class="input input-bordered input-sm flex-1" placeholder="Name, z.B. MUNBW 2027" bind:value={newName} />
			<button
				class="btn btn-primary btn-sm"
				onclick={addConference}
				disabled={saving || !newName.trim()}
				aria-label="Konferenz anlegen"
			>
				<i class="fa-solid fa-plus"></i>
			</button>
		</div>
	</div>
</div>
