use ftbbl;

INSERT INTO Team (name, race_id, coach, is_active)
	SELECT  "The Government", id, "Theamazingzorro", 1 FROM Race WHERE Name="Lizardmen" UNION ALL
    SELECT  "Scooby Snacks", id, "Danean", 1 FROM Race WHERE Name="Necromantic" UNION ALL
    SELECT  "Sedie Incollate", id, "Sampittu_Nature", 1 FROM Race WHERE Name="Undead"
;