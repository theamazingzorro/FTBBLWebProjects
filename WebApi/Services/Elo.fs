namespace ftbbl.WebApi.Services

module Elo =
    open ftbbl.WebApi.Models

    type gameResult = HomeWin | HomeLoss | Draw | Unknown

    let resultOfGame (game : Game): gameResult =
        if not game.HomeScore.HasValue || not game.AwayScore.HasValue then Unknown
        else 

        let homeScore : int = game.HomeScore.Value
        let awayScore : int = game.AwayScore.Value

        if homeScore > awayScore then HomeWin
        elif homeScore < awayScore then HomeLoss
        else Draw


    let winningOdds (r1 : int) (r2 : int) : float =
        let rating1 : float = float r1
        let rating2 : float = float r2

        1. / (1. + (10. ** ((rating2 - rating1) / 400.)))


    let newRatings (r1 : int) (r2 : int) (result : gameResult) : (int * int) =
        let K = 40.

        let prob1Wins = winningOdds r1 r2
        let prob2Wins = winningOdds r2 r1

        match result with
        | HomeWin ->
            (r1 + int (K * (1. - prob1Wins)), r2 + int (K * (0. - prob2Wins)))

        | HomeLoss ->
            (r1 + int (K * (0. - prob1Wins)), r2 + int (K * (1. - prob2Wins)))

        | Draw ->
            (r1 + int (K * (0.5 - prob1Wins)), r2 + int (K * (0.5 - prob2Wins)))

        | Unknown ->
            (r1, r2)