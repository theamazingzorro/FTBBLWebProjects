namespace ftbbl.WebApi.Repositories

module TeamEloHistoryRepository = 
    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]


    let getByTeam(id : int) =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<TeamEloHistory>("""
                SELECT * FROM TeamEloHistory
                WHERE TeamEloHistory.id=@0""", id)
            |> List.ofSeq

        

    let save (history : TeamEloHistory) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<TeamEloHistory>(history)   