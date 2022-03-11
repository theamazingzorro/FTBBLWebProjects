namespace ftbbl.WebApi.Repositories

module CoachRepository =

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

        db.Fetch<Coach>("""
                SELECT * FROM Coach
                WHERE Coach.is_active
                """)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Coach>("""
                SELECT * FROM Coach
                WHERE Coach.is_active
                AND Coach.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name=""; Elo=0 }