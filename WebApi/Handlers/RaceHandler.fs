namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module RaceApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.RaceApiHandlers"


    let getRaces : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Races"
                
                let races = RaceRepository.getAll()

                return! json races next ctx
            }


    let getRace (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Race: id={id}"
                
                let race = RaceRepository.getById id

                return! json race next ctx
            }