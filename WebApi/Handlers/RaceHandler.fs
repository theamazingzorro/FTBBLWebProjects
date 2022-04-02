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
                
                let coachs = RaceRepository.getAll()

                return! json coachs next ctx
            }


    let getRace (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Race: id={id}"
                
                let coach = RaceRepository.getById id

                return! json coach next ctx
            }