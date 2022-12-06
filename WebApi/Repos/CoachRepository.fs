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
                SELECT 
                    Coach.*,
                    COUNT(Accolade.id) as AccoladeCount
                FROM Coach
                LEFT JOIN Accolade ON Accolade.coach_id=Coach.id
                GROUP BY Coach.id
                """)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<Coach>("""
                SELECT 
                    Coach.*,
                    COUNT(Accolade.id) as AccoladeCount
                FROM Coach
                LEFT JOIN Accolade ON Accolade.coach_id=Coach.id
                WHERE Coach.id=@0""", id)


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
