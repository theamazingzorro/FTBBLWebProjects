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

        db.Fetch<Team>("select * from Team where is_active=1")
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Team>("select * from Team where is_active=1 and id=@0", id)
        with
            :? InvalidOperationException -> {Name=""; Race=""; Coach=""; IsActive=false}
        
        
        
        