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

    let getByDiv (divId : int) : Standing list =
        let teams = TeamService.getByDiv divId
        let games = GameService.getByDiv divId

        List.map (getStanding divId games) teams

    let getById (divId : int) (teamId : int) : Standing =
        let team = TeamService.getById teamId
        let games = GameService.getByDiv divId

        getStanding divId games team


