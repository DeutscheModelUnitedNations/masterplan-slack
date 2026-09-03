import mysql from 'mysql2/promise';
import type { RowDataPacket } from 'mysql2';
import { env } from '$env/dynamic/private';

// DATABASE_URL (mysql://user:pass@host:port/db) gewinnt, falls gesetzt -
// sonst einzelne DB_*-Variablen (z.B. fuer docker-compose.yaml).
const pool = env.DATABASE_URL
	? mysql.createPool(env.DATABASE_URL)
	: mysql.createPool({
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
	// Alles (Personen, Tage, Locations, Gruppen) haengt an einer Konferenz -
	// ein neues Jahr/Event faengt einfach mit einer neuen Konferenz bei null an,
	// alte Konferenzen bleiben zum Nachschauen erhalten.
	`CREATE TABLE IF NOT EXISTS conferences (
		id INT AUTO_INCREMENT PRIMARY KEY,
		name VARCHAR(255) NOT NULL
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS people (
		id INT AUTO_INCREMENT PRIMARY KEY,
		conference_id INT NOT NULL,
		name VARCHAR(255) NOT NULL,
		email VARCHAR(255) NULL,
		UNIQUE KEY uniq_people_conference_name (conference_id, name),
		UNIQUE KEY uniq_people_conference_email (conference_id, email),
		FOREIGN KEY (conference_id) REFERENCES conferences(id) ON DELETE CASCADE
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS schedule_days (
		id INT AUTO_INCREMENT PRIMARY KEY,
		conference_id INT NOT NULL,
		label VARCHAR(255) NOT NULL,
		sort_order INT NOT NULL DEFAULT 0,
		FOREIGN KEY (conference_id) REFERENCES conferences(id) ON DELETE CASCADE
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS locations (
		id INT AUTO_INCREMENT PRIMARY KEY,
		conference_id INT NOT NULL,
		name VARCHAR(255) NOT NULL,
		lat DOUBLE NULL,
		lng DOUBLE NULL,
		FOREIGN KEY (conference_id) REFERENCES conferences(id) ON DELETE CASCADE
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS schedule_items (
		id INT AUTO_INCREMENT PRIMARY KEY,
		day_id INT NOT NULL,
		time VARCHAR(32) NOT NULL DEFAULT '',
		end_time VARCHAR(32) NULL,
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
	) ENGINE=InnoDB`,
	// heisst absichtlich nicht "groups" - das ist seit MySQL 8 ein reserviertes Wort
	`CREATE TABLE IF NOT EXISTS person_groups (
		id INT AUTO_INCREMENT PRIMARY KEY,
		conference_id INT NOT NULL,
		name VARCHAR(255) NOT NULL,
		UNIQUE KEY uniq_group_conference_name (conference_id, name),
		FOREIGN KEY (conference_id) REFERENCES conferences(id) ON DELETE CASCADE
	) ENGINE=InnoDB`,
	`CREATE TABLE IF NOT EXISTS person_group_members (
		group_id INT NOT NULL,
		person_id INT NOT NULL,
		PRIMARY KEY (group_id, person_id),
		FOREIGN KEY (group_id) REFERENCES person_groups(id) ON DELETE CASCADE,
		FOREIGN KEY (person_id) REFERENCES people(id) ON DELETE CASCADE
	) ENGINE=InnoDB`
];

// MySQL kennt kein "ADD COLUMN IF NOT EXISTS" (anders als MariaDB) - daher
// vor dem ALTER erst per information_schema pruefen, ob die Spalte schon da ist.
async function addColumnIfMissing(table: string, column: string, definition: string): Promise<void> {
	const [rows] = await pool.query<RowDataPacket[]>(
		`SELECT COUNT(*) AS cnt FROM information_schema.columns
		 WHERE table_schema = DATABASE() AND table_name = ? AND column_name = ?`,
		[table, column]
	);
	if (rows[0].cnt > 0) return;
	await pool.query(`ALTER TABLE ${table} ADD COLUMN ${definition}`);
}

async function ensureSchema(): Promise<void> {
	if (!schemaReady) {
		schemaReady = (async () => {
			for (const statement of SCHEMA_STATEMENTS) {
				await pool.query(statement);
			}
			// end_time kam nachtraeglich dazu - bei bereits existierender Tabelle nachziehen.
			await addColumnIfMissing('schedule_items', 'end_time', 'end_time VARCHAR(32) NULL AFTER time');
		})();
	}
	return schemaReady;
}

export async function db() {
	await ensureSchema();
	return pool;
}
