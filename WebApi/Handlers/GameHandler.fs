namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module GameHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.GameApiHandlers"

    let getGames : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Games"
                
                let games = GameRepository.getAll()

                return! json games next ctx
            }


    let getGamesByDiv (divId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Games for division: div id={divId}"
                
                let games = GameRepository.getByDiv divId

                return! json games next ctx
            }

    let getGame (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Game with id={id}"
                
                let game = GameRepository.getById id

                return! json game next ctx
            }


    let postGame : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! game = ctx.BindJsonAsync<Game>()

                logger.LogInformation $"Saving Game: id={game.Id}"
                
                GameRepository.save(game)

                return! json game next ctx
            }


    let deleteGame (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Game: id={id}"
                let res = GameRepository.deleteById(id)

                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateGame (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! game = ctx.BindJsonAsync<Game>()
                logger.LogInformation $"Updating Game: id={id}"

                GameRepository.save { game with Id = id }

                return! json game next ctx
            }
