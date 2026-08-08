<script lang="ts">
	import type { Group, Person } from '$lib/types';

	let {
		groups,
		people,
		conferenceId,
		reload
	}: { groups: Group[]; people: Person[]; conferenceId: number | null; reload: () => void } = $props();

	let newName = $state('');
	let saving = $state(false);
	let expandedId = $state<number | null>(null);

	async function addGroup() {
		if (!newName.trim() || !conferenceId) return;
		saving = true;
		await fetch('/api/groups', {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ conferenceId, name: newName.trim() })
		});
		newName = '';
		saving = false;
		reload();
	}

	async function removeGroup(id: number) {
		if (!confirm('Gruppe wirklich löschen?')) return;
		await fetch(`/api/groups/${id}`, { method: 'DELETE' });
		reload();
	}

	async function toggleMember(groupId: number, personId: number, member: boolean) {
		await fetch(`/api/groups/${groupId}/members`, {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ personId, member })
		});
		reload();
	}
</script>

<div class="card bg-base-100 shadow-sm border border-base-300">
	<div class="card-body gap-3">
		<h2 class="card-title text-base">Gruppen</h2>
		<p class="text-xs text-base-content/60">
			Zum schnellen Zuordnen mehrerer Personen auf einmal zu einem Programmpunkt.
		</p>

		<div class="flex flex-col gap-1">
			{#each groups as group (group.id)}
				<div class="border border-base-300 rounded-lg">
					<div class="flex items-center gap-2 text-sm w-full p-2">
						<button
							type="button"
							class="flex items-center gap-2 flex-1 min-w-0 text-left"
							onclick={() => (expandedId = expandedId === group.id ? null : group.id)}
						>
							<i class="fa-solid {expandedId === group.id ? 'fa-chevron-down' : 'fa-chevron-right'} text-xs"></i>
							<span class="flex-1 truncate">{group.name}</span>
							<span class="text-xs text-base-content/50">{group.personIds.length} Personen</span>
						</button>
						<button
							type="button"
							class="btn btn-ghost btn-xs text-error"
							aria-label="Gruppe löschen"
							onclick={() => removeGroup(group.id)}
						>
							<i class="fa-solid fa-trash"></i>
						</button>
					</div>
					{#if expandedId === group.id}
						<div class="grid grid-cols-2 gap-1 p-2 pt-0 max-h-40 overflow-auto">
							{#each people as p (p.id)}
								<label class="label cursor-pointer justify-start gap-2 text-sm">
									<input
										type="checkbox"
										class="checkbox checkbox-xs"
										checked={group.personIds.includes(p.id)}
										onchange={(e) => toggleMember(group.id, p.id, (e.target as HTMLInputElement).checked)}
									/>
									<span class="truncate">{p.name}</span>
								</label>
							{/each}
						</div>
					{/if}
				</div>
			{:else}
				<p class="text-sm text-base-content/60">
					{conferenceId ? 'Noch keine Gruppen angelegt.' : 'Erst eine Konferenz auswählen.'}
				</p>
			{/each}
		</div>

		<div class="flex gap-2">
			<input
				class="input input-bordered input-sm flex-1"
				placeholder="Name, z.B. GV"
				bind:value={newName}
				disabled={!conferenceId}
			/>
			<button
				class="btn btn-primary btn-sm"
				onclick={addGroup}
				disabled={saving || !newName.trim() || !conferenceId}
				aria-label="Gruppe hinzufügen"
			>
				<i class="fa-solid fa-plus"></i>
			</button>
		</div>
	</div>
</div>
