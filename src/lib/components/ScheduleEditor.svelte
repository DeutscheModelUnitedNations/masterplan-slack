<script lang="ts">
	import type { Group, Location, Person, ScheduleDay, ScheduleItem } from '$lib/types';
	import LocationMap from './LocationMap.svelte';
	import ScheduleCalendar from './ScheduleCalendar.svelte';

	let {
		people,
		locations,
		groups,
		conferenceId,
		selectedDayId = $bindable(null)
	}: {
		people: Person[];
		locations: Location[];
		groups: Group[];
		conferenceId: number | null;
		selectedDayId: number | null;
	} = $props();

	let days = $state<ScheduleDay[]>([]);
	let items = $state<ScheduleItem[]>([]);
	let newDayLabel = $state('');
	let loadingItems = $state(false);

	async function loadDays() {
		if (!conferenceId) {
			days = [];
			return;
		}
		const res = await fetch(`/api/schedule/days?conferenceId=${conferenceId}`);
		const data = await res.json();
		days = data.days;
		if (selectedDayId == null && days.length > 0) selectedDayId = days[0].id;
	}

	async function loadItems() {
		if (!selectedDayId) {
			items = [];
			return;
		}
		loadingItems = true;
		const res = await fetch(`/api/schedule/items?dayId=${selectedDayId}`);
		const data = await res.json();
		items = data.items;
		loadingItems = false;
	}

	$effect(() => {
		conferenceId;
		selectedDayId = null;
		loadDays();
	});
	$effect(() => {
		selectedDayId;
		loadItems();
	});

	async function addDay() {
		if (!newDayLabel.trim() || !conferenceId) return;
		const res = await fetch('/api/schedule/days', {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ conferenceId, label: newDayLabel.trim() })
		});
		const data = await res.json();
		newDayLabel = '';
		await loadDays();
		selectedDayId = data.day.id;
	}

	async function removeDay(id: number) {
		if (!confirm('Tag inkl. aller Programmpunkte wirklich löschen?')) return;
		await fetch(`/api/schedule/days/${id}`, { method: 'DELETE' });
		if (selectedDayId === id) selectedDayId = null;
		await loadDays();
	}

	// ---- Programmpunkt-Modal ---------------------------------------------
	let showItemModal = $state(false);
	let editingItem = $state<ScheduleItem | null>(null);
	let itemTime = $state('');
	let itemEndTime = $state('');
	let itemTitle = $state('');
	let itemLocationId = $state<number | null>(null);
	let itemTeamInfo = $state(false);
	let itemPersonIds = $state<Set<number>>(new Set());
	let personFilter = $state('');
	let groupToAdd = $state<number | null>(null);
	// Wiederkehrende Ereignisse: beim Neuanlegen zusaetzlich auf diesen Tagen anlegen.
	let repeatOnDayIds = $state<Set<number>>(new Set());

	function openNewItem() {
		editingItem = null;
		itemTime = '';
		itemEndTime = '';
		itemTitle = '';
		itemLocationId = null;
		itemTeamInfo = false;
		itemPersonIds = new Set();
		personFilter = '';
		groupToAdd = null;
		repeatOnDayIds = new Set();
		showItemModal = true;
	}

	function openEditItem(item: ScheduleItem) {
		editingItem = item;
		itemTime = item.time;
		itemEndTime = item.endTime ?? '';
		itemTitle = item.title;
		itemLocationId = item.locationId;
		itemTeamInfo = item.teamInfo;
		itemPersonIds = new Set(item.personIds);
		personFilter = '';
		groupToAdd = null;
		repeatOnDayIds = new Set();
		showItemModal = true;
	}

	function toggleRepeatDay(id: number) {
		const next = new Set(repeatOnDayIds);
		if (next.has(id)) next.delete(id);
		else next.add(id);
		repeatOnDayIds = next;
	}

	function togglePerson(id: number) {
		const next = new Set(itemPersonIds);
		if (next.has(id)) next.delete(id);
		else next.add(id);
		itemPersonIds = next;
	}

	function addGroupMembers() {
		if (groupToAdd == null) return;
		const group = groups.find((g) => g.id === groupToAdd);
		if (!group) return;
		const next = new Set(itemPersonIds);
		for (const id of group.personIds) next.add(id);
		itemPersonIds = next;
	}

	let selectedLocation = $derived(locations.find((l) => l.id === itemLocationId) ?? null);

	async function saveItem() {
		if (!selectedDayId) return;
		const body = {
			dayId: selectedDayId,
			time: itemTime.trim(),
			endTime: itemEndTime.trim() || null,
			title: itemTitle.trim(),
			locationId: itemLocationId,
			teamInfo: itemTeamInfo,
			...(editingItem ? {} : { repeatOnDayIds: [...repeatOnDayIds] })
		};

		let itemId = editingItem?.id;
		let repeatedIds: number[] = [];
		if (editingItem) {
			await fetch(`/api/schedule/items/${editingItem.id}`, {
				method: 'PATCH',
				headers: { 'content-type': 'application/json' },
				body: JSON.stringify(body)
			});
		} else {
			const res = await fetch('/api/schedule/items', {
				method: 'POST',
				headers: { 'content-type': 'application/json' },
				body: JSON.stringify(body)
			});
			const data = await res.json();
			itemId = data.id;
			repeatedIds = data.repeatedIds ?? [];
		}

		const before = new Set(editingItem?.personIds ?? []);
		const toAdd = [...itemPersonIds].filter((id) => !before.has(id));
		const toRemove = [...before].filter((id) => !itemPersonIds.has(id));
		const affectedIds = [itemId!, ...repeatedIds];
		await Promise.all(
			affectedIds.flatMap((id) => [
				...toAdd.map((personId) => setAssignment(id, personId, true)),
				...toRemove.map((personId) => setAssignment(id, personId, false))
			])
		);

		showItemModal = false;
		await loadItems();
	}

	function setAssignment(itemId: number, personId: number, assigned: boolean) {
		return fetch(`/api/schedule/items/${itemId}/assignment`, {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify({ personId, assigned })
		});
	}

	async function removeItem(id: number) {
		if (!confirm('Programmpunkt wirklich löschen?')) return;
		await fetch(`/api/schedule/items/${id}`, { method: 'DELETE' });
		await loadItems();
	}

	let filteredPeople = $derived(people.filter((p) => p.name.toLowerCase().includes(personFilter.toLowerCase())));
</script>

<div class="card bg-base-100 shadow-sm border border-base-300">
	<div class="card-body gap-3">
		<h2 class="card-title text-base">Zeitplan verwalten</h2>

		{#if !conferenceId}
			<p class="text-sm text-base-content/60">Erst eine Konferenz auswählen.</p>
		{:else}
			<div class="flex flex-wrap items-center gap-2">
				<select class="select select-bordered select-sm" bind:value={selectedDayId}>
					{#each days as day (day.id)}
						<option value={day.id}>{day.label}</option>
					{/each}
				</select>
				{#if selectedDayId}
					<button class="btn btn-ghost btn-sm text-error" onclick={() => removeDay(selectedDayId!)}>
						<i class="fa-solid fa-trash"></i> Tag löschen
					</button>
				{/if}
				<div class="flex gap-2 ml-auto">
					<input class="input input-bordered input-sm" placeholder="Neuer Tag, z.B. Tag 1 – Mittwoch" bind:value={newDayLabel} />
					<button class="btn btn-outline btn-sm" onclick={addDay} disabled={!newDayLabel.trim()}>
						<i class="fa-solid fa-plus"></i> Tag anlegen
					</button>
				</div>
			</div>

			{#if !selectedDayId}
				<p class="text-sm text-base-content/60">Zuerst einen Tag anlegen.</p>
			{:else}
			{#if loadingItems}
				<span class="loading loading-spinner loading-sm"></span>
			{:else}
				<ScheduleCalendar
					{items}
					{people}
					editable
					emptyMessage="Noch keine Programmpunkte für diesen Tag."
					onEdit={openEditItem}
					onDelete={removeItem}
				/>
			{/if}
			<button class="btn btn-primary btn-sm w-fit" onclick={openNewItem}>
				<i class="fa-solid fa-plus"></i> Programmpunkt hinzufügen
			</button>
		{/if}
		{/if}
	</div>
</div>

{#if showItemModal}
	<div class="modal modal-open">
		<div class="modal-box max-w-2xl">
			<h3 class="font-bold text-lg mb-4">{editingItem ? 'Programmpunkt bearbeiten' : 'Neuer Programmpunkt'}</h3>

			<div class="grid grid-cols-2 sm:grid-cols-4 gap-3">
				<fieldset class="fieldset">
					<legend class="fieldset-legend">Von</legend>
					<input class="input input-bordered w-full" placeholder="09:00" bind:value={itemTime} />
				</fieldset>
				<fieldset class="fieldset">
					<legend class="fieldset-legend">Bis (optional)</legend>
					<input class="input input-bordered w-full" placeholder="10:30" bind:value={itemEndTime} />
				</fieldset>
				<fieldset class="fieldset col-span-2">
					<legend class="fieldset-legend">Programmpunkt</legend>
					<input class="input input-bordered w-full" bind:value={itemTitle} />
				</fieldset>
				<fieldset class="fieldset sm:col-span-2">
					<legend class="fieldset-legend">Ort</legend>
					<select class="select select-bordered w-full" bind:value={itemLocationId}>
						<option value={null}>Keine Location</option>
						{#each locations as loc (loc.id)}
							<option value={loc.id}>{loc.name}</option>
						{/each}
					</select>
				</fieldset>
				<label class="label cursor-pointer gap-2 self-end pb-3 sm:col-span-2">
					<input type="checkbox" class="checkbox checkbox-sm" bind:checked={itemTeamInfo} />
					<span class="label-text">Team-Info anzeigen</span>
				</label>
			</div>

			{#if !editingItem && days.length > 1}
				<div class="mt-3">
					<span class="text-sm font-semibold">Wiederkehrend – zusätzlich anlegen an:</span>
					<div class="flex flex-wrap gap-2 mt-1">
						{#each days.filter((d) => d.id !== selectedDayId) as day (day.id)}
							<label class="label cursor-pointer gap-2 border border-base-300 rounded-lg px-2 py-1">
								<input
									type="checkbox"
									class="checkbox checkbox-sm"
									checked={repeatOnDayIds.has(day.id)}
									onchange={() => toggleRepeatDay(day.id)}
								/>
								<span class="label-text">{day.label}</span>
							</label>
						{/each}
					</div>
				</div>
			{/if}

			{#if selectedLocation}
				<div class="mt-3">
					<LocationMap lat={selectedLocation.lat} lng={selectedLocation.lng} height="160px" />
				</div>
			{/if}

			{#if groups.length > 0}
				<div class="flex items-center gap-2 mt-4">
					<select class="select select-bordered select-sm flex-1" bind:value={groupToAdd}>
						<option value={null}>Gruppe übernehmen…</option>
						{#each groups as group (group.id)}
							<option value={group.id}>{group.name} ({group.personIds.length})</option>
						{/each}
					</select>
					<button class="btn btn-outline btn-sm" onclick={addGroupMembers} disabled={groupToAdd == null}>
						<i class="fa-solid fa-users"></i> Hinzufügen
					</button>
				</div>
			{/if}

			<div class="mt-4">
				<div class="flex items-center justify-between mb-1">
					<span class="text-sm font-semibold">Personen</span>
					<input class="input input-bordered input-xs" placeholder="Filtern…" bind:value={personFilter} />
				</div>
				<div class="grid grid-cols-2 sm:grid-cols-3 gap-1 max-h-56 overflow-auto border border-base-300 rounded-lg p-2">
					{#each filteredPeople as p (p.id)}
						<label class="label cursor-pointer justify-start gap-2 text-sm">
							<input
								type="checkbox"
								class="checkbox checkbox-sm"
								checked={itemPersonIds.has(p.id)}
								onchange={() => togglePerson(p.id)}
							/>
							<span class="truncate">{p.name}</span>
						</label>
					{/each}
				</div>
			</div>

			<div class="modal-action">
				<button class="btn" onclick={() => (showItemModal = false)}>Abbrechen</button>
				<button class="btn btn-primary" onclick={saveItem} disabled={!itemTitle.trim()}>Speichern</button>
			</div>
		</div>
		<button class="modal-backdrop" onclick={() => (showItemModal = false)} aria-label="Schließen"></button>
	</div>
{/if}
