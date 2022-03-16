namespace ftbbl.WebApi.Repositories

module TeamRepository = 
    open ftbbl.WebApi.Models.Team

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
                WHERE Team.is_active
                """)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Team>("""
                SELECT * FROM Team
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
                WHERE Team.is_active
                AND Team.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name=""; Race={Id=0; Name=""; IsActive=false}; Coach={Id=0; Name=""; Elo=0; IsActive=false}; Elo=0; IsActive=false }
        

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


    let update (team : Team) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Team>(team)
        
        