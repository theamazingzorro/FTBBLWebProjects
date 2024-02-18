namespace ftbbl.WebApi.Repositories

module CoachRepository =

    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open System
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]

    let getCoachSQL (whereClause : string) :string = $"""
        SELECT 
            Coach.*, 
            max(season) as recent_season 
        FROM Coach
        LEFT JOIN Team on Team.coach_id=Coach.id
        LEFT JOIN TeamDivision on Team.id=TeamDivision.team_id
        LEFT JOIN Division on TeamDivision.div_id=Division.id
        {whereClause}
        GROUP BY Coach.id;
    """

    let getAll(leagueId : int) =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Coach>(getCoachSQL "WHERE Coach.league_id=@0", leagueId)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<Coach>(getCoachSQL "WHERE Coach.id=@0", id)


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
