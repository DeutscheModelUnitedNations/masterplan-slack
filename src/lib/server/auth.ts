import { error } from '@sveltejs/kit';
import { env } from '$env/dynamic/private';

// Die Auth-Middleware "dmun-team-auth" ist traefik-forward-auth
// (github.com/thomseddon/traefik-forward-auth) mit Google als Provider. Sie
// loggt Nutzer per Google ein und reicht die E-Mail im X-Forwarded-User-Header
// durch - dafuer muss die Traefik-Middleware "authResponseHeaders=X-Forwarded-User"
// gesetzt haben, sonst kommt der Header hier nie an.
export function emailFromRequest(request: Request): string | null {
	const headerName = env.AUTH_EMAIL_HEADER || 'X-Forwarded-User';
	const email = request.headers.get(headerName);
	return email?.trim().toLowerCase() || null;
}

export function isAdmin(email: string | null): boolean {
	if (!email) return false;
	const admins = (env.ADMIN_EMAILS ?? '')
		.split(',')
		.map((e) => e.trim().toLowerCase())
		.filter(Boolean);
	return admins.includes(email);
}

export function requireAdmin(locals: App.Locals): void {
	if (!locals.isAdmin) error(403, 'Diese Aktion ist nur für Admins.');
}
