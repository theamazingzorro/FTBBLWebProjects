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
            , COUNT(TeamAccolade.id) as AccoladeCount
            , COUNT(CoachAccolade.id) as Coach__AccoladeCount
        FROM Team
        JOIN Race ON Team.race_id=Race.id
        JOIN Coach ON Team.coach_id=Coach.id
        LEFT JOIN (
	        SELECT a.* FROM TeamDivision a
		        LEFT JOIN TeamDivision b
			        ON a.team_id=b.team_id 
			        AND (a.start_date < b.start_date 
				        OR (a.start_date = b.start_date AND a.end_date > b.end_date)
				        OR (a.start_date = b.start_date AND a.end_date IS NULL AND b.end_date IS NOT NULL)
			        )
		        WHERE b.start_date IS NULL
	        ) TD ON Team.id=TD.team_id
        LEFT JOIN Division ON TD.div_id=Division.id
        LEFT JOIN Accolade TeamAccolade ON TeamAccolade.team_id=Team.id
        LEFT JOIN Accolade CoachAccolade ON CoachAccolade.coach_id=Coach.id
        {whereClause}
        GROUP BY Team.id
    """


    let getAll() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL "")
            |> List.ofSeq


    let getFree() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL """
                WHERE Team.id NOT IN 
	                (SELECT Team.id FROM Team
	                JOIN TeamDivision ON Team.id = TeamDivision.team_id
	                WHERE TeamDivision.end_date is null)
                """)
            |> List.ofSeq


    let getByDiv (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL """ 
                JOIN TeamDivision on TeamDivision.team_id=Team.id
                WHERE TeamDivision.div_id=@0""", divId)
            |> List.ofSeq


    let getNotInDiv (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>(getTeamSQL """
                WHERE Team.id NOT IN 
	                (SELECT Team.id FROM Team
	                JOIN TeamDivision ON Team.id = TeamDivision.team_id
	                WHERE TeamDivision.div_id=@0)
                """, divId)
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

        
        