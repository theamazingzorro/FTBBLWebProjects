namespace ftbbl.WebApi.Services

open System


module StandingService =
    open ftbbl.WebApi.Models

    let private getStanding (divId : int) (games : Game list) (team : Team) : Standing=
        let homeGames = List.where (fun game -> team.Id = game.HomeTeam.Id) games
        let unplayedHomeGames = List.where (fun game -> Elo.gameResult.Unknown = Elo.resultOfGame game) homeGames
        let awayGames = List.where (fun game -> team.Id = game.AwayTeam.Id) games
        let unplayedAwayGames = List.where (fun game -> Elo.gameResult.Unknown = Elo.resultOfGame game) awayGames
        
        let remainingElos = (List.map (fun game -> float game.AwayTeam.Elo) unplayedHomeGames
                    |> List.append (List.map (fun game -> float game.HomeTeam.Elo) unplayedAwayGames ))

        {
            DivId = divId; 
            Team = team; 
            Rank = 0;
            Wins = List.where (fun game -> Elo.gameResult.HomeWin = Elo.resultOfGame game) homeGames
                    |> List.append (List.where (fun game -> Elo.gameResult.HomeLoss = Elo.resultOfGame game) awayGames)
                    |> List.length;
            Losses = List.where (fun game -> Elo.gameResult.HomeLoss = Elo.resultOfGame game) homeGames
                    |> List.append (List.where (fun game -> Elo.gameResult.HomeWin = Elo.resultOfGame game) awayGames)
                    |> List.length;
            Draws = List.append homeGames awayGames
                    |> List.where (fun game -> Elo.gameResult.Draw = Elo.resultOfGame game)
                    |> List.length; 
            PointsGiven = List.map (fun game -> game.AwayScore.GetValueOrDefault(0)) homeGames
                    |> List.append (List.map (fun game -> game.HomeScore.GetValueOrDefault(0)) awayGames)
                    |> List.sum;
            PointsScored = List.map (fun game -> game.HomeScore.GetValueOrDefault(0)) homeGames
                    |> List.append (List.map (fun game -> game.AwayScore.GetValueOrDefault(0)) awayGames)
                    |> List.sum;
            AvgRemainingElo = (
                    if List.length remainingElos > 0 
                        then Nullable<int> (int <| List.average remainingElos)
                        else Nullable()
                    )
        }

    let private sortRankDesc (a : Standing) (b : Standing) =
        match compare (3*b.Wins + b.Draws) (3*a.Wins + a.Draws) with
            | 0 -> match compare (b.PointsScored - b.PointsGiven) (a.PointsScored - a.PointsGiven) with
                    | 0 -> match compare b.PointsScored a.PointsScored with
                            | 0 -> match compare b.PointsGiven a.PointsGiven with
                                    | 0 -> compare b.Team.Elo a.Team.Elo
                                    | z -> z
                            | z -> z
                    | y -> y
            | x -> x

    let getByDiv (divId : int) : Standing list =
        let teams = TeamService.getByDiv divId
        let games = GameService.getByDiv divId

        List.map (getStanding divId games) teams
        |> List.sortWith sortRankDesc
        |> List.mapi (fun index standing -> { standing with Rank = index + 1 } )

    let getById (divId : int) (teamId : int) : Standing =
        getByDiv divId
        |> List.filter (fun standing -> standing.Team.Id = teamId)
        |> List.head

    // need to keep an eye on performance here
    // this could make a lot of db calls for older teams
    let getAllForTeam (teamId : int) : DivStanding list =
        DivisionService.getAllForTeam teamId
        |> List.map (fun div -> 
                let standing = getById div.Id teamId
                {   
                    Div = div;
                    TeamId = standing.Team.Id;
                    Rank = standing.Rank;
                    Wins = standing.Wins;
                    Draws = standing.Draws;
                    Losses = standing.Losses;
                    PointsGiven = standing.PointsGiven;
                    PointsScored = standing.PointsScored
                })


