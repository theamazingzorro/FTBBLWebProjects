namespace ftbbl.WebApi.Repositories

open System.Configuration
open Microsoft.AspNetCore.Builder



module TeamRepository = 
    open ftbbl.WebApi.Models

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

        db.Single<Team>("select * from Team where is_active=1 and id=@0", id)
        
        
        