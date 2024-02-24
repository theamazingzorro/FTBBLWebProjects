namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module GameHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.GameApiHandlers"

    let getGames : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Games"
                
                let result = GameService.getAll()

                return! json result next ctx
            }


    let getGamesByDiv (divId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Games for division: div id={divId}"
                
                let result = GameService.getByDiv divId

                return! json result next ctx
            }

    let getGamesByTeams (team1Id : int, team2Id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Head-to-Head for 2 teams: t1 id={team1Id}, t2 id={team2Id}"
                
                let result = GameService.getByTeams team1Id team2Id

                return! json result next ctx
            }

    let getGamesByCoaches (coach1Id : int, coach2Id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Head-to-Head for 2 coaches: c1 id={coach1Id}, c2 id={coach2Id}"
                
                let result = GameService.getByCoaches coach1Id coach2Id

                return! json result next ctx
            }

    let getGame (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Game with id={id}"
                
                let result = GameService.getById id

                return! json result next ctx
            }


    let postGame : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! game = ctx.BindJsonAsync<Game>()

                logger.LogInformation $"Saving New Game: id={game.Id}, homeTeamId={game.HomeTeam.Id}, awayTeamId{game.AwayTeam.Id}, week={game.Week}"
                
                let result = GameService.saveNew game

                return! json result next ctx
            }


    let deleteGame (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Game: id={id}"
                let res = GameService.deleteById id

                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateGame (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! game = ctx.BindJsonAsync<Game>()
                logger.LogInformation $"Updating Game: id={id}"

                let result = GameService.saveOverId id game

                return! json result next ctx
            }
