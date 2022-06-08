namespace ftbbl.WebApi.Repositories

module CoachEloHistoryRepository = 
    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]


    let getByCoach(id : int) =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<CoachEloHistory>("""
                SELECT * FROM CoachEloHistory
                WHERE CoachEloHistory.id=@0""", id)
            |> List.ofSeq

        

    let save (history : CoachEloHistory) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<CoachEloHistory>(history)   