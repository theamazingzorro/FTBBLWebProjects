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
                WHERE is_active=1
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
                WHERE is_active=1
                AND Team.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name=""; Race={Id=0; Name=""}; Coach="" }
        
        
        
        