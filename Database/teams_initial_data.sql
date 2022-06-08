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

INSERT INTO Division (name, closed, season)
	SELECT "Div A", True, 1 UNION ALL
    SELECT "Div B", False, 1 UNION ALL
    SELECT "Div A", False, 2
;

INSERT INTO TeamDivision (team_id, div_id, start_date, end_date)
	SELECT Team.id, Division.id, date('2020-01-01'), date('2021-01-01') 
		FROM (Team, Division) WHERE Team.name="The Government" AND Division.name="Div A" and Division.season=1 UNION ALL
    SELECT Team.id, Division.id, date('2020-01-01'), NULL 
		FROM (Team, Division) WHERE Team.name="Scooby Snacks" AND Division.name="Div B" and Division.season=1 UNION ALL
    SELECT Team.id, Division.id, date('2021-01-01'), NULL 
		FROM (Team, Division) WHERE Team.name="The Government" AND Division.name="Div A" and Division.season=2
;