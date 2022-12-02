namespace ftbbl.WebApi.Repositories

module TeamRepository = 
    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open System
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]


    let getAll() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>("""
                SELECT * FROM Team
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
                """)
            |> List.ofSeq


    let getFree() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>("""
                SELECT * FROM Team
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
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

        db.Fetch<Team>("""
                SELECT * FROM Team
                JOIN TeamDivision ON Team.id = TeamDivision.team_id
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
                WHERE TeamDivision.div_id=@0
                """, divId)
            |> List.ofSeq


    let getNotInDiv (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>("""
                SELECT * FROM Team
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
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

        db.SingleOrDefault<Team>("""
            SELECT * FROM Team
            JOIN Race ON Team.race_id=Race.id
            JOIN Coach ON Team.coach_id=Coach.id
            WHERE Team.id=@0""", id)
        

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

        
        