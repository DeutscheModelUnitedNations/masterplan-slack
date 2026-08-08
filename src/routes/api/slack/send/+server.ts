import { json } from '@sveltejs/kit';
import { sendMessages, summarizeSend } from '$lib/server/slack';

export async function POST({ request }) {
	const { entries, test } = (await request.json()) as {
		entries: { recipient: string; text: string }[];
		test: boolean;
	};

	if (entries.length === 0) {
		return json({ message: 'Es sind keine Empfänger zum Senden ausgewählt.' });
	}

	const results = await sendMessages(entries, test);
	return json({ message: summarizeSend(results) });
}
