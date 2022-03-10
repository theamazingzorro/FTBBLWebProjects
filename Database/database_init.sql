create database if not exists ftbbl;
use ftbbl;

create table if not exists Race(
	id INT NOT NULL AUTO_INCREMENT,
    name VARCHAR(40) NOT NULL UNIQUE,
    is_active BOOL NOT NULL,
    PRIMARY KEY ( id )
);

INSERT INTO Race(name, is_active)
    SELECT t.name, true
    FROM (
		SELECT "Amazon" AS name UNION ALL
		SELECT "Bretonnian" AS name UNION ALL
		SELECT "Chaos Dwarf" AS name UNION ALL
		SELECT "Dark Elves" AS name UNION ALL
		SELECT "Dwarf" AS name UNION ALL
		SELECT "Lizardmen" AS name UNION ALL
		SELECT "Norse" AS name UNION ALL
		SELECT "Orc" AS name UNION ALL
		SELECT "Skaven" AS name UNION ALL
		SELECT "Undead" AS name UNION ALL
		SELECT "Wood Elves" AS name UNION ALL
		SELECT "Chaos" AS name UNION ALL
		SELECT "Pro Elves" AS name UNION ALL
		SELECT "High Elves" AS name UNION ALL
		SELECT "Humans" AS name UNION ALL
		SELECT "Khemri" AS name UNION ALL
		SELECT "Necromantic" AS name UNION ALL
		SELECT "Nurgle" AS name UNION ALL
		SELECT "Underworld" AS name UNION ALL
		SELECT "Kislev Circus" AS name UNION ALL
		SELECT "Vampire" AS name UNION ALL
		SELECT "Halfling" AS name UNION ALL
		SELECT "Goblin" AS name UNION ALL
		SELECT "Ogre" AS name
         ) t
    WHERE NOT EXISTS (SELECT 1 FROM Race);


create table if not exists Team(
   id INT NOT NULL AUTO_INCREMENT,
   name VARCHAR(40) NOT NULL UNIQUE,
   race_id INT NOT NULL,
   coach VARCHAR(40) NOT NULL,
   is_active BOOL NOT NULL,
   FOREIGN KEY ( race_id ) REFERENCES Race(id),
   PRIMARY KEY ( id )
);