namespace ftbbl.WebApi.Repositories

module AccoladeRepository = 
    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]


    let getAll() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Accolade>("SELECT * FROM Accolade")
            |> List.ofSeq


    let getAllForTeam (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Accolade>("SELECT * FROM Accolade WHERE Accolade.team_id=@0", id)
            |> List.ofSeq


    let getAllForCoach (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Accolade>("SELECT * FROM Accolade WHERE Accolade.coach_id=@0", id)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<Accolade>("SELECT * FROM Accolade WHERE Accolade.id=@0", id)
        

    let save (accolade : Accolade) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Accolade>(accolade)   


    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try
            db.DeleteWhere<Accolade>("Accolade.id=@0", id)
        with
            | :? MySqlException -> 0


        
        