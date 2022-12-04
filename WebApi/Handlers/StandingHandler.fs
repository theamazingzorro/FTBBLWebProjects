namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module StandingHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.StandingApiHandlers"

    let getDivStandings (divId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Standings with divId={divId}"
                
                let result = StandingService.getByDiv divId

                return! json result next ctx
            }

    let getStanding (divId : int, teamId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Standing with divId={divId}, teamId={teamId}"
                
                let result = StandingService.getById divId teamId

                return! json result next ctx
            }
