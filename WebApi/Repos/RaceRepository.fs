namespace ftbbl.WebApi.Repositories

module RaceRepository =

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

        db.Fetch<Race>("""
                SELECT * FROM Race
                """)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Race>("""
                SELECT * FROM Race
                WHERE Race.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name="" }