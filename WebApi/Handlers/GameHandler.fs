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

                logger.LogInformation $"Saving Game: id={game.Id}"
                
                let result = GameService.saveChanges game

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
