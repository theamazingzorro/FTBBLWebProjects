namespace ftbbl.WebApi.Repositories

module TeamRepository = 
    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open System
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]

    let getTeamSQL (whereClause : string) :string = $"""
        SELECT 
	          Team.*
	        , Race.*
	        , Coach.*
	        , Division.*
        FROM Team
        JOIN Race ON Team.race_id=Race.id
        JOIN Coach ON Team.coach_id=Coach.id
        LEFT JOIN (
	        SELECT a.* FROM TeamDivision a
	            LEFT JOIN TeamDivision b
		            ON a.team_id=b.team_id 
		            AND (a.start_date < b.start_date 
			            OR (a.start_date <> b.start_date AND b.end_date IS NULL)
                        OR (a.start_date = b.start_date AND a.end_date IS NOT NULL AND b.end_date IS NULL)
			            OR (a.start_date = b.start_date AND a.end_date < b.end_date)
                        OR (a.start_date = b.start_date AND a.end_date = b.end_date AND a.div_id > b.div_id)
		            )
	            WHERE b.start_date IS NULL
	        ) TD ON Team.id=TD.team_id
        LEFT JOIN Division ON TD.div_id=Division.id
        {whereClause}
    """


    let getAll(leagueId : int) =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL " WHERE Team.league_id = @0", leagueId)
            |> List.ofSeq


    let getFree(leagueId : int) =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL """
                WHERE Team.id NOT IN 
	                (SELECT Team.id FROM Team
	                JOIN TeamDivision ON Team.id = TeamDivision.team_id
	                WHERE TeamDivision.end_date is null)
                AND Team.league_id = @0
                """, leagueId)
            |> List.ofSeq

    let getByCoach (coachId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL " WHERE Team.coach_id=@0", coachId)
            |> List.ofSeq
        

    let getByDiv (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL """ 
                JOIN TeamDivision on TeamDivision.team_id=Team.id
                WHERE TeamDivision.div_id=@0""", divId)
            |> List.ofSeq


    let getNotInDiv (leagueId : int, divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL """
                WHERE Team.id NOT IN 
	                (SELECT Team.id FROM Team
	                JOIN TeamDivision ON Team.id = TeamDivision.team_id
	                WHERE TeamDivision.div_id=@0)
                AND Team.league_id=@1
                """, divId, leagueId)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<Team>(getTeamSQL " WHERE Team.id=@0", id)
        

    let save (team : Team) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Team>(team)   


    let updateDiv (teamId : int) (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Execute("CALL sp_UpdateTeamDivision(@0, @1)", teamId, divId)

    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.DeleteWhere<Team>("Team.id=@0", id)

        
        