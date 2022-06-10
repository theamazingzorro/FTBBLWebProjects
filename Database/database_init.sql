CREATE DATABASE IF NOT EXISTS ftbbl;
USE ftbbl;


-- Race --

CREATE TABLE IF NOT EXISTS Race(
	id INT NOT NULL AUTO_INCREMENT,
	name VARCHAR(40) NOT NULL UNIQUE,
	PRIMARY KEY ( id )
);

INSERT INTO Race(name)
	SELECT t.name
	FROM (
		SELECT "Amazon" AS name UNION ALL
		SELECT "Bretonnian" AS name UNION ALL
		SELECT "Chaos" AS name UNION ALL
		SELECT "Chaos Dwarf" AS name UNION ALL
		SELECT "Dark Elves" AS name UNION ALL
		SELECT "Dwarf" AS name UNION ALL
		SELECT "Goblin" AS name UNION ALL
		SELECT "Halfling" AS name UNION ALL
		SELECT "High Elves" AS name UNION ALL
		SELECT "Humans" AS name UNION ALL
		SELECT "Khemri" AS name UNION ALL
		SELECT "Kislev Circus" AS name UNION ALL
		SELECT "Lizardmen" AS name UNION ALL
		SELECT "Necromantic" AS name UNION ALL
		SELECT "Norse" AS name UNION ALL
		SELECT "Nurgle" AS name UNION ALL
		SELECT "Ogre" AS name UNION ALL
		SELECT "Orc" AS name UNION ALL
		SELECT "Pro Elves" AS name UNION ALL
		SELECT "Skaven" AS name UNION ALL
		SELECT "Undead" AS name UNION ALL
		SELECT "Underworld" AS name UNION ALL
		SELECT "Vampire" AS name UNION ALL
		SELECT "Wood Elves" AS name 
		 ) t
	WHERE NOT EXISTS (SELECT 1 FROM Race);


-- Coach --

CREATE TABLE IF NOT EXISTS Coach(
	id INT NOT NULL AUTO_INCREMENT,
	name VARCHAR(40) NOT NULL UNIQUE,
	elo INT NOT NULL,
	PRIMARY KEY ( id )
);


-- Team --

CREATE TABLE IF NOT EXISTS Team(
	id INT NOT NULL AUTO_INCREMENT,
	name VARCHAR(40) NOT NULL UNIQUE,
	race_id INT NOT NULL,
	coach_id INT NOT NULL,
	elo INT NOT NULL,
	FOREIGN KEY ( race_id ) REFERENCES Race(id),
	FOREIGN KEY ( coach_id ) REFERENCES Coach(id),
	PRIMARY KEY ( id )
);


-- Divisions --

CREATE TABLE IF NOT EXISTS Division(
	id INT NOT NULL AUTO_INCREMENT,
	name VARCHAR(40) NOT NULL,
	season INT NOT NULL,
    closed BOOL NOT NULL,
	UNIQUE ( name, season),
	PRIMARY KEY ( id )
);


CREATE TABLE IF NOT EXISTS TeamDivision(
	team_id INT NOT NULL,
	div_id INT NOT NULL,
	start_date DATE NOT NULL,
	end_date DATE NULL,
    FOREIGN KEY ( team_id ) REFERENCES Team(id),
    FOREIGN KEY ( div_id ) REFERENCES Division(id),
	PRIMARY KEY ( team_id, div_id )
);


-- Game --

CREATE TABLE IF NOT EXISTS Game(
	id INT NOT NULL AUTO_INCREMENT,
	home_team_id INT NOT NULL,
    away_team_id INT NOT NULL,
    div_id INT NOT NULL,
    home_score INT,
    away_score INT,
    week INT,
    FOREIGN KEY ( home_team_id ) REFERENCES Team(id),
    FOREIGN KEY ( away_team_id ) REFERENCES Team(id),
    FOREIGN KEY ( div_id ) REFERENCES Division(id),
    PRIMARY KEY ( id )
);


-- ELO History --

CREATE TABLE IF NOT EXISTS TeamEloHistory(
	id INT NOT NULL AUTO_INCREMENT,
    team_id INT NOT NULL,
    elo INT NOT NULL,
    date DATETIME NOT NULL,
    FOREIGN KEY ( team_id ) REFERENCES Team(id),
    PRIMARY KEY ( id )
);


CREATE TABLE IF NOT EXISTS CoachEloHistory(
	id INT NOT NULL AUTO_INCREMENT,
    coach_id INT NOT NULL,
    elo INT NOT NULL,
    date DATETIME NOT NULL,
    FOREIGN KEY ( coach_id ) REFERENCES Coach(id),
    PRIMARY KEY ( id )
);


-- Users --

CREATE TABLE IF NOT EXISTS User(
    username VARCHAR(100),
    password VARCHAR(100),
    is_admin BIT,
    PRIMARY KEY ( username )
);