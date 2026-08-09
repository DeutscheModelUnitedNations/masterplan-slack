<script lang="ts">
	import type { Person, ScheduleItem } from '$lib/types';
	import LocationMap from './LocationMap.svelte';

	let {
		items,
		people = [],
		editable = false,
		emptyMessage = 'Noch keine Programmpunkte.',
		onEdit,
		onDelete
	}: {
		items: ScheduleItem[];
		people?: Person[];
		editable?: boolean;
		emptyMessage?: string;
		onEdit?: (item: ScheduleItem) => void;
		onDelete?: (id: number) => void;
	} = $props();

	const HOUR_HEIGHT = 64; // px pro Stunde in der Zeitachse

	function parseMinutes(t: string | null | undefined): number | null {
		if (!t) return null;
		const m = /^(\d{1,2}):(\d{2})/.exec(t.trim());
		if (!m) return null;
		const h = Number(m[1]);
		const mm = Number(m[2]);
		if (h > 23 || mm > 59) return null;
		return h * 60 + mm;
	}

	interface LayoutEvent {
		id: number;
		start: number;
		end: number;
		hasEnd: boolean;
		item: ScheduleItem;
		col: number;
		totalCols: number;
	}

	// Ueberlappende Ereignisse (z.B. parallele Programmpunkte an verschiedenen Orten)
	// nebeneinander in Spalten anordnen - klassischer Cluster+Greedy-Spalten-Ansatz
	// wie bei Tages-Kalenderansichten ueblich.
	function layoutDay(events: Omit<LayoutEvent, 'col' | 'totalCols'>[]): LayoutEvent[] {
		const sorted = [...events].sort((a, b) => a.start - b.start || a.end - b.end);
		const result: LayoutEvent[] = [];
		let cluster: typeof sorted = [];
		let clusterEnd = -Infinity;
		const clusters: (typeof sorted)[] = [];

		for (const e of sorted) {
			if (cluster.length && e.start >= clusterEnd) {
				clusters.push(cluster);
				cluster = [];
				clusterEnd = -Infinity;
			}
			cluster.push(e);
			clusterEnd = Math.max(clusterEnd, e.end);
		}
		if (cluster.length) clusters.push(cluster);

		for (const cl of clusters) {
			const columnEnds: number[] = [];
			const colOf = new Map<number, number>();
			for (const e of cl) {
				let placed = false;
				for (let c = 0; c < columnEnds.length; c++) {
					if (columnEnds[c] <= e.start) {
						columnEnds[c] = e.end;
						colOf.set(e.id, c);
						placed = true;
						break;
					}
				}
				if (!placed) {
					columnEnds.push(e.end);
					colOf.set(e.id, columnEnds.length - 1);
				}
			}
			const totalCols = columnEnds.length;
			for (const e of cl) {
				result.push({ ...e, col: colOf.get(e.id)!, totalCols });
			}
		}
		return result;
	}

	let parsed = $derived(
		items.map((item) => {
			const start = parseMinutes(item.time);
			const end = item.endTime ? parseMinutes(item.endTime) : null;
			return { item, start, end };
		})
	);
	let withoutTime = $derived(parsed.filter((p) => p.start == null).map((p) => p.item));
	let withTime = $derived(
		parsed.filter((p): p is { item: ScheduleItem; start: number; end: number | null } => p.start != null)
	);

	let gridStartHour = $derived.by(() => {
		if (withTime.length === 0) return 8;
		return Math.max(0, Math.floor(Math.min(...withTime.map((t) => t.start)) / 60));
	});
	let gridEndHour = $derived.by(() => {
		if (withTime.length === 0) return 20;
		const maxMin = Math.max(...withTime.map((t) => (t.end ?? t.start + 30)));
		return Math.min(24, Math.max(gridStartHour + 1, Math.ceil(maxMin / 60)));
	});
	let hours = $derived.by(() => {
		const arr: number[] = [];
		for (let h = gridStartHour; h <= gridEndHour; h++) arr.push(h);
		return arr;
	});

	let laidOut = $derived(
		layoutDay(
			withTime.map((t) => ({
				id: t.item.id,
				start: t.start,
				end: t.end ?? t.start + 30,
				hasEnd: t.end != null,
				item: t.item
			}))
		)
	);

	let selected = $state<ScheduleItem | null>(null);

	function personName(id: number) {
		return people.find((p) => p.id === id)?.name ?? `#${id}`;
	}
</script>

<div class="flex flex-col gap-2">
	{#if items.length === 0}
		<p class="text-sm text-base-content/60">{emptyMessage}</p>
	{:else}
		{#if withoutTime.length > 0}
			<div class="flex flex-col gap-1">
				{#each withoutTime as item (item.id)}
					<button
						type="button"
						class="flex items-center gap-2 text-left text-sm border border-base-300 rounded-lg px-2 py-1 hover:bg-base-200"
						onclick={() => (selected = item)}
					>
						<span class="badge badge-ghost badge-sm shrink-0">ohne Zeit</span>
						<span class="font-medium truncate">{item.title}</span>
						{#if item.teamInfo}<span class="badge badge-ghost badge-xs shrink-0">Team-Info</span>{/if}
					</button>
				{/each}
			</div>
		{/if}

		<div class="flex" style="height: {(gridEndHour - gridStartHour) * HOUR_HEIGHT}px">
			<div class="relative w-12 shrink-0">
				{#each hours as h (h)}
					<span
						class="absolute right-2 -translate-y-1/2 text-xs text-base-content/50"
						style="top: {(h - gridStartHour) * HOUR_HEIGHT}px"
					>
						{String(h).padStart(2, '0')}:00
					</span>
				{/each}
			</div>
			<div class="relative flex-1 border-l border-base-300 min-w-0">
				{#each hours as h (h)}
					<div class="absolute left-0 right-0 border-t border-base-300/60" style="top: {(h - gridStartHour) * HOUR_HEIGHT}px"></div>
				{/each}
				{#each laidOut as ev (ev.id)}
					<button
						type="button"
						class="absolute rounded-lg border border-primary/40 bg-primary/10 hover:bg-primary/20 text-left px-2 py-1 overflow-hidden transition-colors"
						style="top: {(ev.start - gridStartHour * 60) / 60 * HOUR_HEIGHT}px;
						       height: {Math.max((ev.end - ev.start) / 60 * HOUR_HEIGHT, 26)}px;
						       left: calc({(ev.col / ev.totalCols) * 100}% + 2px);
						       width: calc({100 / ev.totalCols}% - 4px);"
						onclick={() => (selected = ev.item)}
					>
						<div class="text-xs font-mono text-primary leading-tight">
							{ev.item.time}{#if ev.hasEnd}–{ev.item.endTime}{/if}
						</div>
						<div class="text-sm font-medium leading-tight truncate">{ev.item.title}</div>
					</button>
				{/each}
			</div>
		</div>
	{/if}
</div>

{#if selected}
	<div class="modal modal-open">
		<div class="modal-box">
			<h3 class="font-bold text-lg mb-1">{selected.title}</h3>
			<p class="text-sm text-base-content/60 mb-3 font-mono">
				{selected.time || '–'}{#if selected.endTime}–{selected.endTime}{/if}
			</p>

			{#if selected.teamInfo}
				<span class="badge badge-ghost badge-sm mb-3">Team-Info</span>
			{/if}

			{#if selected.location}
				<div class="mb-3">
					<div class="font-semibold text-sm mb-1 flex items-center gap-1">
						<i class="fa-solid fa-location-dot text-base-content/60"></i>
						{selected.location.name}
					</div>
					{#if selected.location.lat != null && selected.location.lng != null}
						<LocationMap lat={selected.location.lat} lng={selected.location.lng} height="160px" />
					{/if}
				</div>
			{/if}

			{#if people.length > 0 && selected.personIds.length > 0}
				<div>
					<div class="font-semibold text-sm mb-1 flex items-center gap-1">
						<i class="fa-solid fa-users text-base-content/60"></i>
						Personen
					</div>
					<div class="flex flex-wrap gap-1">
						{#each selected.personIds as pid (pid)}
							<span class="badge badge-outline badge-sm">{personName(pid)}</span>
						{/each}
					</div>
				</div>
			{/if}

			<div class="modal-action">
				{#if editable}
					<button
						class="btn btn-ghost text-error"
						onclick={() => {
							onDelete?.(selected!.id);
							selected = null;
						}}
					>
						<i class="fa-solid fa-trash"></i> Löschen
					</button>
					<button
						class="btn btn-outline"
						onclick={() => {
							onEdit?.(selected!);
							selected = null;
						}}
					>
						<i class="fa-solid fa-pen"></i> Bearbeiten
					</button>
				{/if}
				<button class="btn" onclick={() => (selected = null)}>Schließen</button>
			</div>
		</div>
		<button class="modal-backdrop" onclick={() => (selected = null)} aria-label="Schließen"></button>
	</div>
{/if}
