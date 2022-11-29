namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module RaceHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.RaceApiHandlers"


    let getRaces : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Races"
                
                let result = RaceService.getAll()

                return! json result next ctx
            }


    let getRace (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Race: id={id}"
                
                let result = RaceService.getById id

                return! json result next ctx
            }