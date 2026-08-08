import type { Handle } from '@sveltejs/kit';
import { emailFromRequest, isAdmin } from '$lib/server/auth';

export const handle: Handle = async ({ event, resolve }) => {
	const email = emailFromRequest(event.request);
	event.locals.email = email;
	event.locals.isAdmin = isAdmin(email);
	return resolve(event);
};
