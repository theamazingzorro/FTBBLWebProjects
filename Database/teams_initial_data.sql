USE ftbbl;

INSERT INTO Coach (name, elo)
    SELECT "Theamazingzorro", 1000 UNION ALL
    SELECT "Danean", 1000
;

INSERT INTO Team (name, race_id, coach_id, elo)
    SELECT  "The Government", Race.id, Coach.id, 1000 FROM (Race, Coach) WHERE Race.name="Lizardmen" AND Coach.name="Theamazingzorro" UNION ALL
    SELECT  "Scooby Snacks", Race.id, Coach.id, 1000 FROM (Race, Coach) WHERE Race.name="Necromantic" AND Coach.name="Danean" UNION ALL
    SELECT  " Murder Hobos", Race.id, Coach.id, 1000 FROM (Race, Coach) WHERE Race.name="Chaos" AND Coach.name="Theamazingzorro"
;