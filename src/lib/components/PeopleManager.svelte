<script lang="ts">
	import type { Person } from '$lib/types';

	let {
		people,
		conferenceId,
		reload
	}: { people: Person[]; conferenceId: number | null; reload: () => void } = $props();

	let newName = $state('');
	let newEmail = $state('');
	let saving = $state(false);

	let editingId = $state<number | null>(null);
	let editName = $state('');
	let editEmail = $state('');

	async function addPerson() {
		if (!newName.trim() || !conferenceId) return;
		saving = true;
		await fetch('/api/people', {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ conferenceId, name: newName.trim(), email: newEmail.trim() || null })
		});
		newName = '';
		newEmail = '';
		saving = false;
		reload();
	}

	function startEdit(p: Person) {
		editingId = p.id;
		editName = p.name;
		editEmail = p.email ?? '';
	}

	async function saveEdit() {
		if (editingId == null) return;
		await fetch(`/api/people/${editingId}`, {
			method: 'PATCH',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ name: editName.trim(), email: editEmail.trim() || null })
		});
		editingId = null;
		reload();
	}

	async function removePerson(id: number) {
		if (!confirm('Person wirklich löschen? Zuordnungen im Zeitplan gehen dabei verloren.')) return;
		await fetch(`/api/people/${id}`, { method: 'DELETE' });
		reload();
	}
</script>

<div class="card bg-base-100 shadow-sm border border-base-300">
	<div class="card-body gap-3">
		<h2 class="card-title text-base">Personen</h2>

		<div class="flex flex-col gap-2 max-h-72 overflow-auto">
			{#each people as p (p.id)}
				<div class="flex items-center gap-2 text-sm">
					{#if editingId === p.id}
						<input class="input input-bordered input-sm flex-1" bind:value={editName} />
						<input
							class="input input-bordered input-sm flex-1"
							placeholder="E-Mail (optional)"
							bind:value={editEmail}
						/>
						<button class="btn btn-success btn-xs" onclick={saveEdit} aria-label="Speichern">
							<i class="fa-solid fa-check"></i>
						</button>
						<button class="btn btn-ghost btn-xs" onclick={() => (editingId = null)} aria-label="Abbrechen">
							<i class="fa-solid fa-xmark"></i>
						</button>
					{:else}
						<span class="flex-1 truncate">{p.name}</span>
						<span class="text-xs text-base-content/50 truncate">{p.email ?? ''}</span>
						<button class="btn btn-ghost btn-xs" onclick={() => startEdit(p)} aria-label="Bearbeiten">
							<i class="fa-solid fa-pen"></i>
						</button>
						<button
							class="btn btn-ghost btn-xs text-error"
							onclick={() => removePerson(p.id)}
							aria-label="Löschen"
						>
							<i class="fa-solid fa-trash"></i>
						</button>
					{/if}
				</div>
			{:else}
				<p class="text-sm text-base-content/60">
					{conferenceId ? 'Noch keine Personen angelegt.' : 'Erst eine Konferenz auswählen.'}
				</p>
			{/each}
		</div>

		<div class="flex gap-2">
			<input class="input input-bordered input-sm flex-1" placeholder="Name" bind:value={newName} disabled={!conferenceId} />
			<input
				class="input input-bordered input-sm flex-1"
				placeholder="E-Mail (optional)"
				bind:value={newEmail}
				disabled={!conferenceId}
			/>
			<button
				class="btn btn-primary btn-sm"
				onclick={addPerson}
				disabled={saving || !newName.trim() || !conferenceId}
				aria-label="Person hinzufügen"
			>
				<i class="fa-solid fa-plus"></i>
			</button>
		</div>
		<p class="text-xs text-base-content/60">
			E-Mail ist optional – ohne sie wird beim Login automatisch nach dem Schema
			<code>v.nachname@dmun.de</code> gematcht. Bei Unsicherheit lieber hier direkt eine E-Mail hinterlegen.
		</p>
	</div>
</div>
