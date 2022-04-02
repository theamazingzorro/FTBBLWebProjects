namespace ftbbl.WebApi.Repositories

module CoachRepository =

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

        db.Fetch<Coach>("""
                SELECT * FROM Coach
                """)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Coach>("""
                SELECT * FROM Coach
                WHERE Coach.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name=""; Elo=0 }


    let save (coach : Coach) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Coach>(coach)


    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.DeleteWhere<Coach>("Coach.id=@0", id)


    let update (coach : Coach) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Coach>(coach)