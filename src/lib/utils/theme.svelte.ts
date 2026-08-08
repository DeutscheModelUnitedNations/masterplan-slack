// Kleiner Theme-Store fuer den Hell/Dunkel-Umschalter.
// Die Vorab-Ermittlung (vor dem ersten Paint) passiert inline in app.html,
// hier wird nur noch synchron gehalten und umgeschaltet.

type Theme = 'light' | 'dark' | 'system';

function systemPrefersDark() {
	return window.matchMedia('(prefers-color-scheme: dark)').matches;
}

function resolve(theme: Theme): 'light' | 'dark' {
	return theme === 'system' ? (systemPrefersDark() ? 'dark' : 'light') : theme;
}

class ThemeStore {
	stored: Theme = $state('system');
	resolved: 'light' | 'dark' = $state('light');

	constructor() {
		if (typeof localStorage !== 'undefined') {
			const saved = localStorage.getItem('theme') as Theme | null;
			this.stored = saved ?? 'system';
		}
		this.resolved = typeof window !== 'undefined' ? resolve(this.stored) : 'light';
	}

	set(theme: Theme) {
		this.stored = theme;
		this.resolved = resolve(theme);
		localStorage.setItem('theme', theme);
		document.documentElement.setAttribute('data-theme', this.resolved);
	}

	toggle() {
		this.set(this.resolved === 'dark' ? 'light' : 'dark');
	}
}

export const theme = new ThemeStore();
