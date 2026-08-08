<script lang="ts">
	import { onMount } from 'svelte';
	import { theme } from '$lib/utils/theme.svelte';
	import PeopleManager from '$lib/components/PeopleManager.svelte';
	import ScheduleEditor from '$lib/components/ScheduleEditor.svelte';
	import { WORKSPACES, type MessageEntry, type Person, type ScheduleDay, type ScheduleItem, type SlackStatus, type SlackUser, type Workspace } from '$lib/types';
	import type { PageData } from './$types';

	let { data }: { data: PageData } = $props();

	async function postJSON<T>(url: string, body: unknown): Promise<T> {
		const res = await fetch(url, {
			method: 'POST',
			headers: { 'content-type': 'application/json' },
			body: JSON.stringify(body)
		});
		if (!res.ok) {
			const err = await res.json().catch(() => ({ message: 'Es ist ein Fehler aufgetreten.' }));
			throw new Error(err.message ?? 'Es ist ein Fehler aufgetreten.');
		}
		return res.json();
	}

	// ---- Schritt 1: Verbindung -------------------------------------------
	let status = $state<SlackStatus | null>(null);
	let users = $state<SlackUser[]>([]);
	let refreshing = $state(false);
	let showWorkspaceModal = $state(false);

	async function refreshStatus() {
		const res = await fetch('/api/status');
		const data = await res.json();
		status = data.status;
	}

	async function loadUsers() {
		const res = await fetch('/api/slack/users');
		const data = await res.json();
		users = data.users;
	}

	async function changeWorkspace(ws: Workspace) {
		showWorkspaceModal = false;
		const data = await postJSON<{ status: SlackStatus }>('/api/workspace', { workspace: ws });
		status = data.status;
		await loadUsers();
	}

	async function refreshSlack() {
		refreshing = true;
		const res = await fetch('/api/slack/refresh', { method: 'POST' });
		const data = await res.json();
		status = data.status;
		await loadUsers();
		refreshing = false;
	}

	// ---- Personen + Zeitplan (nur Admins) ------------------------------------
	let people = $state<Person[]>([]);
	let selectedDayId = $state<number | null>(null);

	async function loadPeople() {
		const res = await fetch('/api/people');
		const data = await res.json();
		people = data.people;
	}

	onMount(() => {
		refreshStatus();
		loadUsers();
		if (data.isAdmin) loadPeople();
		else loadMySchedule();
	});

	// ---- Schritt 3: Nachrichten erstellen & pruefen --------------------------
	let messages = $state<MessageEntry[] | null>(null);
	let picks = $state<Record<number, string>>({});
	let includes = $state<Record<number, boolean>>({});
	let building = $state(false);

	async function build() {
		if (!selectedDayId) return;
		building = true;
		try {
			const data = await postJSON<{ messages: MessageEntry[] }>('/api/schedule/build', {
				dayId: selectedDayId
			});
			messages = data.messages;
			picks = {};
			includes = {};
		} finally {
			building = false;
		}
	}

	function recipientOf(i: number, entry: MessageEntry) {
		return picks[i] ?? entry.nameSlack ?? '';
	}

	function includedOf(i: number, entry: MessageEntry) {
		return includes[i] ?? entry.include;
	}

	let hasEmpty = $derived((messages ?? []).some((e) => e.nItems === 0));

	function excludeEmpty() {
		(messages ?? []).forEach((e, i) => {
			if (e.nItems === 0) includes[i] = false;
		});
	}

	function includedEntries() {
		return (messages ?? []).map((entry, i) => ({ entry, i })).filter(({ entry, i }) => includedOf(i, entry));
	}

	function messageText(entry: MessageEntry) {
		return `${entry.messageHead}\n${entry.messageBody}`;
	}

	// ---- Schritt 4: Versenden -------------------------------------------------
	let testMode = $state(true);
	let sending = $state(false);
	let sendStatus = $state<string | null>(null);
	let showConfirmModal = $state(false);

	let confirmSummary = $derived.by(() => {
		const included = includedEntries();
		return {
			total: included.length,
			good: included.filter((x) => x.entry.matchQuality === 'good').length,
			weak: included.filter((x) => x.entry.matchQuality === 'weak').length,
			poor: included.filter((x) => x.entry.matchQuality === 'poor').length,
			empty: included.filter((x) => x.entry.nItems === 0).length
		};
	});

	async function doSend() {
		const entries = includedEntries().map(({ entry, i }) => ({
			recipient: recipientOf(i, entry),
			text: messageText(entry)
		}));
		sending = true;
		try {
			const data = await postJSON<{ message: string }>('/api/slack/send', { entries, test: testMode });
			sendStatus = data.message;
		} catch (e) {
			sendStatus = e instanceof Error ? e.message : 'Versand fehlgeschlagen.';
		} finally {
			sending = false;
		}
	}

	function clickSend() {
		if (testMode) {
			doSend();
		} else {
			showConfirmModal = true;
		}
	}

	// ---- Nicht-Admin: nur der eigene Zeitplan ---------------------------------
	let myMatch = $state<{ name: string; confident: boolean } | null>(null);
	let myDays = $state<{ day: ScheduleDay; items: ScheduleItem[] }[]>([]);
	let myLoaded = $state(false);

	async function loadMySchedule() {
		const res = await fetch('/api/schedule/mine');
		const data = await res.json();
		myMatch = data.match;
		myDays = data.days;
		myLoaded = true;
	}
</script>

<svelte:head>
	<title>DMUN TMK-Bot</title>
</svelte:head>

<div class="min-h-screen bg-base-200">
	<!-- ---- Titelleiste ---------------------------------------------------- -->
	<header class="navbar bg-base-100 border-b border-base-300 px-4">
		<div class="flex-1 flex items-center gap-3">
			<i class="fa-duotone fa-calendar-clock text-2xl text-primary"></i>
			<div>
				<div class="font-bold text-lg leading-tight">TMK-Bot</div>
				<div class="text-sm text-base-content/60">Personalisierte Zeitpläne über Slack versenden</div>
			</div>
		</div>
		{#if data.email}
			<span class="text-sm text-base-content/60 mr-2">{data.email}</span>
		{/if}
		<button class="btn btn-ghost btn-circle" onclick={() => theme.toggle()} aria-label="Hell/Dunkel umschalten">
			<i class="fa-solid {theme.resolved === 'dark' ? 'fa-sun' : 'fa-moon'}"></i>
		</button>
	</header>

	{#if !data.isAdmin}
		<!-- ---- Nicht-Admin: read-only eigener Zeitplan ---------------------------- -->
		<div class="max-w-3xl mx-auto p-4 flex flex-col gap-4">
			<div class="card bg-base-100 shadow-sm border border-base-300">
				<div class="card-body gap-3">
					<h2 class="card-title text-base">Mein Zeitplan</h2>
					{#if !myLoaded}
						<span class="loading loading-spinner loading-sm"></span>
					{:else if !myMatch}
						<p class="text-base-content/60">
							Für {data.email} wurde kein Zeitplan gefunden. Bitte bei den Organisator:innen melden, damit die
							Zuordnung ergänzt wird.
						</p>
					{:else}
						<p class="text-sm text-base-content/60">Zeitplan für <strong>{myMatch.name}</strong></p>
						{#if myDays.length === 0}
							<p class="text-base-content/60">Noch keine Programmpunkte für dich eingetragen.</p>
						{/if}
						{#each myDays as { day, items } (day.id)}
							<div class="mt-2">
								<h3 class="font-semibold mb-1">{day.label}</h3>
								<div class="overflow-x-auto">
									<table class="table table-sm">
										<thead>
											<tr>
												<th>Uhrzeit</th>
												<th>Programmpunkt</th>
												<th>Ort</th>
											</tr>
										</thead>
										<tbody>
											{#each items as item (item.id)}
												<tr>
													<td>{item.time}</td>
													<td>{item.title}</td>
													<td>{item.location}</td>
												</tr>
											{/each}
										</tbody>
									</table>
								</div>
							</div>
						{/each}
					{/if}
				</div>
			</div>
		</div>
	{:else}
		<div class="flex flex-col lg:flex-row gap-4 p-4 max-w-7xl mx-auto">
			<!-- ---- Sidebar: Konfiguration ---------------------------------------- -->
			<aside class="w-full lg:w-96 flex flex-col gap-4 shrink-0">
				<!-- Step 1 -->
				<div class="card bg-base-100 shadow-sm border border-base-300">
					<div class="card-body gap-3">
						<h2 class="card-title text-base gap-2">
							<span
								class="w-6 h-6 rounded-full bg-primary text-primary-content text-xs font-bold flex items-center justify-center shrink-0"
								>1</span
							>
							Verbindung
						</h2>

						{#if status?.connected}
							<div class="flex items-center gap-2 text-sm">
								<span class="w-2.5 h-2.5 rounded-full bg-success"></span>
								<span class="font-semibold">Verbunden</span>
							</div>
							<div class="text-sm">
								Workspace <strong>{status.team}</strong> · User <strong>{status.user}</strong>
							</div>
							{#if status.age}
								<div class="text-xs text-base-content/60">Daten aktualisiert: {status.age}</div>
							{/if}
						{:else if status}
							<div class="flex items-center gap-2 text-sm">
								<span class="w-2.5 h-2.5 rounded-full bg-error"></span>
								<span class="font-semibold">Nicht verbunden</span>
							</div>
						{:else}
							<span class="loading loading-spinner loading-sm"></span>
						{/if}

						<div class="grid gap-2">
							<button class="btn btn-outline btn-primary btn-sm" onclick={() => (showWorkspaceModal = true)}>
								<i class="fa-solid fa-right-left"></i> Workspace wechseln
							</button>
							<button class="btn btn-outline btn-secondary btn-sm" onclick={refreshSlack} disabled={refreshing}>
								{#if refreshing}
									<span class="loading loading-spinner loading-xs"></span>
								{:else}
									<i class="fa-solid fa-rotate"></i>
								{/if}
								Slack-Daten aktualisieren
							</button>
						</div>
					</div>
				</div>

				<!-- Step 2 -->
				<PeopleManager {people} reload={loadPeople} />
			</aside>

			<!-- ---- Hauptbereich: Erstellen, pruefen, versenden ---------------------- -->
			<main class="flex-1 flex flex-col gap-4 min-w-0">
				<ScheduleEditor {people} bind:selectedDayId />

				<!-- Step 3 -->
				<div class="card bg-base-100 shadow-sm border border-base-300">
					<div class="card-body gap-3 min-h-[540px]">
						<h2 class="card-title text-base gap-2">
							<span
								class="w-6 h-6 rounded-full bg-primary text-primary-content text-xs font-bold flex items-center justify-center shrink-0"
								>3</span
							>
							Nachrichten erstellen & prüfen
						</h2>
						<p class="text-sm text-base-content/60 -mt-2">
							Treffer prüfen, Empfänger ggf. korrigieren, dann versenden.
						</p>

						<div class="flex justify-between items-center flex-wrap gap-2 mb-2">
							<button class="btn btn-outline btn-sm" onclick={excludeEmpty} disabled={!hasEmpty}>
								<i class="fa-solid fa-eraser"></i> Leere Zeitpläne ausschließen
							</button>
							<button class="btn btn-primary" onclick={build} disabled={!selectedDayId || building}>
								{#if building}
									<span class="loading loading-spinner loading-sm"></span>
								{:else}
									<i class="fa-solid fa-wand-magic-sparkles"></i>
								{/if}
								Nachrichten erstellen
							</button>
						</div>

						{#if !messages}
							<p class="text-base-content/60">
								Noch keine Nachrichten erstellt. Tag wählen und auf „Nachrichten erstellen" klicken.
							</p>
						{:else}
							<datalist id="slack-users">
								{#each users as u (u.id)}
									<option value={u.realName}></option>
								{/each}
							</datalist>
							<div class="flex flex-col gap-2">
								{#each messages as entry, i (entry.personName)}
									<div class="card bg-base-200/50 border border-base-300">
										<div class="card-body p-4 gap-2">
											<div class="flex justify-between items-start flex-wrap gap-2">
												<div>
													<div class="font-semibold">{entry.personName}</div>
													<div class="text-sm text-base-content/60">
														Bester Treffer: {entry.nameSlack ?? '—'} · {Math.round(entry.matchScore * 100)}% Ähnlichkeit
													</div>
												</div>
												<div class="flex gap-1">
													{#if entry.matchQuality === 'good'}
														<span class="badge badge-success">Sicher</span>
													{:else if entry.matchQuality === 'weak'}
														<span class="badge badge-warning">Unsicher</span>
													{:else}
														<span class="badge badge-error">Kein guter Treffer</span>
													{/if}
													{#if entry.nItems === 0}
														<span class="badge badge-ghost">Leerer Zeitplan</span>
													{/if}
												</div>
											</div>

											<div class="grid grid-cols-1 sm:grid-cols-[1fr_auto] gap-3 items-end">
												<fieldset class="fieldset">
													<legend class="fieldset-legend">Slack-Empfänger</legend>
													<input
														class="input input-bordered w-full"
														list="slack-users"
														value={recipientOf(i, entry)}
														oninput={(e) => (picks[i] = (e.target as HTMLInputElement).value)}
													/>
												</fieldset>
												<label class="label cursor-pointer gap-2">
													<span class="label-text">Senden</span>
													<input
														type="checkbox"
														class="toggle toggle-success"
														checked={includedOf(i, entry)}
														onchange={(e) => (includes[i] = (e.target as HTMLInputElement).checked)}
													/>
												</label>
											</div>

											<pre class="tmk-msg text-xs bg-base-100 border border-base-300 rounded-lg p-3 max-h-[340px] overflow-auto whitespace-pre-wrap">{entry.messageHead}
{entry.messageBody}</pre>
										</div>
									</div>
								{/each}
							</div>
						{/if}
					</div>
				</div>

				<!-- Step 4 -->
				<div class="card bg-base-100 shadow-sm border border-base-300">
					<div class="card-body gap-3">
						<h2 class="card-title text-base gap-2">
							<span
								class="w-6 h-6 rounded-full bg-primary text-primary-content text-xs font-bold flex items-center justify-center shrink-0"
								>4</span
							>
							Versenden
						</h2>

						<div class="flex flex-col sm:flex-row items-center gap-4">
							<div class="flex-1">
								<label class="label cursor-pointer justify-start gap-2">
									<input type="checkbox" class="toggle" bind:checked={testMode} />
									<span class="label-text">An Test-Channel senden (kein Versand an echte User)</span>
								</label>
								<p class="text-xs text-base-content/60">
									Test-Schalter zuerst ausschalten, um wirklich zu versenden.
								</p>
							</div>
							<button
								class="btn btn-success btn-lg"
								onclick={clickSend}
								disabled={!messages || messages.length === 0 || sending}
							>
								{#if sending}
									<span class="loading loading-spinner"></span>
								{:else}
									<i class="fa-solid fa-paper-plane"></i>
								{/if}
								Nachrichten versenden
							</button>
						</div>

						{#if sendStatus}
							<div class="alert alert-info" role="status">{sendStatus}</div>
						{/if}
					</div>
				</div>
			</main>
		</div>
	{/if}
</div>

<!-- ---- Modal: Workspace wechseln ------------------------------------------ -->
{#if showWorkspaceModal}
	<div class="modal modal-open">
		<div class="modal-box">
			<h3 class="font-bold text-lg mb-4">Wähle den Workspace aus</h3>
			<div class="flex flex-wrap gap-2">
				{#each WORKSPACES as ws (ws)}
					<button class="btn btn-outline" onclick={() => changeWorkspace(ws)}>{ws}</button>
				{/each}
			</div>
			<div class="modal-action">
				<button class="btn" onclick={() => (showWorkspaceModal = false)}>Abbrechen</button>
			</div>
		</div>
		<button class="modal-backdrop" onclick={() => (showWorkspaceModal = false)} aria-label="Schließen"></button>
	</div>
{/if}

<!-- ---- Modal: Bestaetigung vor echtem Versand ---------------------------------- -->
{#if showConfirmModal}
	<div class="modal modal-open">
		<div class="modal-box">
			<h3 class="font-bold text-lg mb-2">Echte Nachrichten versenden?</h3>
			<p><strong>{confirmSummary.total}</strong> Nachricht(en) gehen an echte Slack-User – nicht an den Test-Channel.</p>
			<ul class="list-disc list-inside text-sm mt-2 space-y-1">
				<li>{confirmSummary.good} sichere Treffer</li>
				<li>{confirmSummary.weak} unsichere Treffer</li>
				<li>{confirmSummary.poor} ohne guten Treffer</li>
				<li>{confirmSummary.empty} mit leerem Zeitplan</li>
			</ul>
			{#if confirmSummary.poor > 0}
				<div class="alert alert-warning mt-3">
					Es gibt Empfänger ohne guten Namens-Treffer. Bitte oben prüfen – diese könnten an die falsche Person gehen.
				</div>
			{/if}
			<div class="modal-action">
				<button class="btn" onclick={() => (showConfirmModal = false)}>Abbrechen</button>
				<button
					class="btn btn-error"
					onclick={() => {
						showConfirmModal = false;
						doSend();
					}}
				>
					Senden bestätigen
				</button>
			</div>
		</div>
		<button class="modal-backdrop" onclick={() => (showConfirmModal = false)} aria-label="Schließen"></button>
	</div>
{/if}
