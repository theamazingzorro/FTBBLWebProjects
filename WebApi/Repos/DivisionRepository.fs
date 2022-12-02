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

        db.SingleOrDefault<Division>("""
            SELECT * FROM Division
            WHERE Division.id=@0""", id)
        

    let save (division : Division) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Division>(division)   


    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try
            db.DeleteWhere<Division>("Division.id=@0", id)
        with
            | :? MySqlException -> 0


    let closeDiv (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Execute("CALL sp_CloseDivision(@0)", divId)


        
        