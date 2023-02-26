use ftbbl;

-- This file is only meant to be run once
-- and is used to bring the production database up to date
-- with the new multiple league system
START TRANSACTION;


-- League Table --

CREATE TABLE League(
	id INT NOT NULL AUTO_INCREMENT,
	name VARCHAR(40) NOT NULL UNIQUE,
	PRIMARY KEY ( id )
);

INSERT INTO League(name)
	SELECT t.name
	FROM (
		SELECT "FTBBL" AS name UNION ALL
		SELECT "Top Scumlord" AS name
		 ) t
	WHERE NOT EXISTS (SELECT 1 FROM League);


-- Table Updates --

ALTER TABLE Coach
	ADD COLUMN league_id INT NOT NULL DEFAULT 1,
    ADD FOREIGN KEY ( league_id ) REFERENCES League(id)
    ;
    
ALTER TABLE Division
	ADD COLUMN league_id INT NOT NULL DEFAULT 1,
    ADD FOREIGN KEY ( league_id ) REFERENCES League(id)
    ;
    
ALTER TABLE Team
	ADD COLUMN league_id INT NOT NULL DEFAULT 1,
    ADD FOREIGN KEY ( league_id ) REFERENCES League(id)
    ;

select * from team t LEFT JOIN league l on t.league_id=l.id;
select * from division t LEFT JOIN league l on t.league_id=l.id;
select * from coach t LEFT JOIN league l on t.league_id=l.id;
ROLLBACK;
-- COMMIT;