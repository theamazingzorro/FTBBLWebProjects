namespace ftbbl.WebApi.Repositories

module CoachRepository =

    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open System
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]

    let getAll(leagueId : int) =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Coach>("""
                SELECT 
                    Coach.*
                FROM Coach
                WHERE Coach.league_id=@0
                """, leagueId)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<Coach>("""
                SELECT 
                    Coach.*
                FROM Coach
                WHERE Coach.id=@0
                """, id)


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
