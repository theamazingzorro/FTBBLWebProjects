namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module CoachApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models.Team
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.CoachApiHandlers"

    let getCoaches =
        fun (getCoaches : unit -> Coach list) (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Coaches"
                
                let coachs = getCoaches()

                return! json coachs next ctx
            }

    let getCoachesHandler : HttpHandler = 
        getCoaches CoachRepository.getAll


    let getCoach (getCoachById : int -> Coach) (id : int) =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Coach: id={id}"
                
                let coach = getCoachById(id)

                return! json coach next ctx
            }

    let getSingleCoachHandler : (int -> HttpHandler) =
        getCoach CoachRepository.getById