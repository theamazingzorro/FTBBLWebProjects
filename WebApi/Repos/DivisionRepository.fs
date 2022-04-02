namespace ftbbl.WebApi.Repositories

module DivisionRepository = 
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

        db.Fetch<Division>("SELECT * FROM Division")
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Division>("""
                SELECT * FROM Division
                WHERE Division.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name=""; Season=0u }
        

    let save (division : Division) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Division>(division)   


    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.DeleteWhere<Division>("Division.id=@0", id)


    let update (division : Division) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Division>(division)
        
        