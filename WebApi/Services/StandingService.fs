namespace ftbbl.WebApi.Services


module StandingService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models

    let private getStanding (divId : int) (games : Game list) (team : Team) : Standing=
        let homeGames = List.where (fun game -> team.Id = game.HomeTeam.Id) games
        let awayGames = List.where (fun game -> team.Id = game.AwayTeam.Id) games
        
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
        }

    let getByDiv (divId : int) : Standing list =
        let teams = TeamRepository.getByDiv divId
        let games = GameRepository.getByDiv divId

        List.map (getStanding divId games) teams

    let getById (divId : int) (teamId : int) : Standing =
        let team = TeamRepository.getById teamId
        let games = GameRepository.getByDiv divId

        getStanding divId games team


