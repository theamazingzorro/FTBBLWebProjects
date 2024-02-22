namespace ftbbl.WebApi.Repositories

module GameRepository = 
    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open System
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]

    let getGameSQL (whereClause : string) :string = $"""
        SELECT 
	        Game.*,
            Division.*,

            HomeTeam.id as HomeTeam__Id,
            HomeTeam.elo as HomeTeam__Elo,
            HomeTeam.name as HomeTeam__Name,
            HomeRace.id as HomeTeam__Race__Id,
            HomeRace.name as HomeTeam__Race__Name,
            HomeCoach.id as HomeTeam__Coach__Id,
            HomeCoach.name as HomeTeam__Coach__Name,
            HomeCoach.elo as HomeTeam__Coach__Elo, 

            AwayTeam.id as AwayTeam__Id,
            AwayTeam.elo as AwayTeam__Elo,
            AwayTeam.name as AwayTeam__Name,
            AwayRace.id as AwayTeam__Race__Id,
            AwayRace.name as AwayTeam__Race__Name,
            AwayCoach.id as AwayTeam__Coach__Id,
            AwayCoach.name as AwayTeam__Coach__Name,
            AwayCoach.elo as AwayTeam__Coach__Elo     
        FROM Game
        JOIN Division ON Game.div_id = Division.id
        JOIN Team AS HomeTeam ON Game.home_team_id = HomeTeam.id
        JOIN Race AS HomeRace ON HomeTeam.race_id = HomeRace.id
        JOIN Coach AS HomeCoach ON HomeTeam.coach_id = HomeCoach.id
        JOIN Team AS AwayTeam ON Game.away_team_id = AwayTeam.id
        JOIN Race AS AwayRace ON AwayTeam.race_id = AwayRace.id
        JOIN Coach AS AwayCoach ON AwayTeam.coach_id = AwayCoach.id

        {whereClause}
    """

    let getAll() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Game>(getGameSQL "")
            |> List.ofSeq


    let getByDiv (divId : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Game>(getGameSQL "WHERE Division.id=@0", divId)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<Game>(getGameSQL "WHERE Game.id=@0", id)


    let getByTeams (team1Id : int) (team2Id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Game>(getGameSQL """
                            WHERE ((HomeTeam.id=@0 AND AwayTeam.id=@1) OR (HomeTeam.id=@1 AND AwayTeam.id=@0))
                            AND Game.home_score IS NOT NULL
                            AND Game.away_score IS NOT NULL
                        """, team1Id, team2Id)
            |> List.ofSeq


    let getByCoaches (coach1Id : int) (coach2Id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Game>(getGameSQL """
                            WHERE ((HomeCoach.id=@0 AND AwayCoach.id=@1) OR (HomeCoach.id=@1 AND AwayCoach.id=@0))
                            AND Game.home_score IS NOT NULL
                            AND Game.away_score IS NOT NULL
                        """, coach1Id, coach2Id)
            |> List.ofSeq


    let save (game : Game) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<Game>(game)   


    let deleteById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.DeleteWhere<Game>("Game.id=@0", id)