import mysql from 'mysql2/promise';
import { env } from '$env/dynamic/private';

const pool = mysql.createPool({
	host: env.DB_HOST || 'localhost',
	port: Number(env.DB_PORT || 3306),
	user: env.DB_USER || 'masterplan',
	password: env.DB_PASSWORD || '',
	database: env.DB_NAME || 'masterplan',
	waitForConnections: true,
	connectionLimit: 10
});

// Schema wird beim ersten Zugriff einmalig sichergestellt - kein separates
// Migrations-Tool noetig fuer eine Handvoll Tabellen.
let schemaReady: Promise<void> | null = null;

const SCHEMA_STATEMENTS = [
	`CREATE TABLE IF NOT EXISTS people (
		id INT AUTO_INCREMENT PRIMARY KEY,
		name VARCHAR(255) NOT NULL UNIQUE,
		email VARCHAR(255) NULL UNIQUE
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS schedule_days (
		id INT AUTO_INCREMENT PRIMARY KEY,
		label VARCHAR(255) NOT NULL,
		sort_order INT NOT NULL DEFAULT 0
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS locations (
		id INT AUTO_INCREMENT PRIMARY KEY,
		name VARCHAR(255) NOT NULL,
		lat DOUBLE NULL,
		lng DOUBLE NULL
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS schedule_items (
		id INT AUTO_INCREMENT PRIMARY KEY,
		day_id INT NOT NULL,
		time VARCHAR(32) NOT NULL DEFAULT '',
		title VARCHAR(255) NOT NULL DEFAULT '',
		location_id INT NULL,
		team_info BOOLEAN NOT NULL DEFAULT FALSE,
		sort_order INT NOT NULL DEFAULT 0,
		FOREIGN KEY (day_id) REFERENCES schedule_days(id) ON DELETE CASCADE,
		FOREIGN KEY (location_id) REFERENCES locations(id) ON DELETE SET NULL
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS schedule_assignments (
		item_id INT NOT NULL,
		person_id INT NOT NULL,
		PRIMARY KEY (item_id, person_id),
		FOREIGN KEY (item_id) REFERENCES schedule_items(id) ON DELETE CASCADE,
		FOREIGN KEY (person_id) REFERENCES people(id) ON DELETE CASCADE
	) ENGINE=InnoDB`
];

async function ensureSchema(): Promise<void> {
	if (!schemaReady) {
		schemaReady = (async () => {
			for (const statement of SCHEMA_STATEMENTS) {
				await pool.query(statement);
			}
		})();
	}
	return schemaReady;
}

export async function db() {
	await ensureSchema();
	return pool;
}
