use ftbbl;

INSERT INTO Coach (name, elo, is_active)
	SELECT "Theamazingzorro", 1000, true UNION ALL
    SELECT "Danean", 1000, true
;

INSERT INTO Team (name, race_id, coach_id, elo, is_active)
	SELECT  "The Government", Race.id, Coach.id, 1000, true FROM (Race, Coach) WHERE Race.name="Lizardmen" AND Coach.name="Theamazingzorro" UNION ALL
    SELECT  "Scooby Snacks", Race.id, Coach.id, 1000, true FROM (Race, Coach) WHERE Race.name="Necromantic" AND Coach.name="Danean" UNION ALL
    SELECT  " Murder Hobos", Race.id, Coach.id, 1000, true FROM (Race, Coach) WHERE Race.name="Chaos" AND Coach.name="Theamazingzorro"
;