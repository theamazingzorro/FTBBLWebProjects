﻿namespace ftbbl.WebApi.Services

module GameService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let getOdds (game:Game) : Game =
        if Elo.resultOfGame game = Elo.gameResult.Unknown 
        then 
            let homeOdds = (70. * Elo.winningOdds game.HomeTeam.Elo game.AwayTeam.Elo
                            + 30. * Elo.winningOdds game.HomeTeam.Coach.Elo game.AwayTeam.Coach.Elo)
            let awayOdds = 100. - homeOdds
            { game with HomeOdds = homeOdds; AwayOdds = awayOdds }
        else game

    let getAll() =
        GameRepository.getAll()
            |> List.map getOdds 

    let getById id =
        GameRepository.getById id
            |> getOdds

    let deleteById id =
        GameRepository.deleteById(id)

    let getByDiv divId =
        GameRepository.getByDiv divId
            |> List.map getOdds

    let getByTeams team1Id team2Id =
        GameRepository.getByTeams team1Id team2Id

    let getByCoaches coach1Id coach2Id =
        GameRepository.getByCoaches coach1Id coach2Id

    let updateElos (game:Game) =
        let homeTeam = game.HomeTeam
        let awayTeam = game.AwayTeam
        let homeCoach = homeTeam.Coach
        let awayCoach = awayTeam.Coach
        let matchResult = Elo.resultOfGame game

        let (htElo, atElo) = Elo.newRatings homeTeam.Elo awayTeam.Elo matchResult
        let (hcElo, acElo) = Elo.newRatings homeCoach.Elo awayCoach.Elo matchResult

        // side effects in my functional code base?
        // its more likely than you may think
        ignore <| TeamService.saveChanges { homeTeam with Elo=htElo }
        ignore <| TeamService.saveChanges { awayTeam with Elo=atElo }

        ignore <| CoachService.saveChanges { homeCoach with Elo=hcElo }
        ignore <| CoachService.saveChanges { awayCoach with Elo=acElo }

        0

    let saveNew (game:Game) =
        GameRepository.save(game)

        game

    let saveChanges (game:Game) = 
        let oldGame = GameRepository.getById game.Id
        
        if Elo.resultOfGame game <> Elo.resultOfGame oldGame 
        then 
            ignore <| updateElos game
            
            GameRepository.save(game)
            game
        else
            GameRepository.save(game)
            game
        

    let saveOverId id game =
        saveChanges { game with Id = id }
