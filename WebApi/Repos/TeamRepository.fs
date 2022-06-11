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


    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.DeleteWhere<Team>("Team.id=@0", id)

        
        