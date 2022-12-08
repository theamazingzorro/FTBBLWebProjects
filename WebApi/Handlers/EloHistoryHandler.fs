namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module EloHistoryHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.EloHistoryHandlers"

    let getTeamHistory (teamId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Elo History for team with id={teamId}"
                
                let result = EloHistoryService.getTeamEloHistory teamId

                return! json result next ctx
            }

    let getCoachHistory (coachId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Elo History for coach with id={coachId}"
                
                let result = EloHistoryService.getCoachEloHistory coachId

                return! json result next ctx
            }
