namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module CoachApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models.Team
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.CoachApiHandlers"

    let getCoaches : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Coaches"
                
                let coachs = CoachRepository.getAll()

                return! json coachs next ctx
            }


    let getCoach (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Coach: id={id}"
                
                let coach = CoachRepository.getById id

                return! json coach next ctx
            }



