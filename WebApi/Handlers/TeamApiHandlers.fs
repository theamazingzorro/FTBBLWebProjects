namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module TeamApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models.Team
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.TeamApiHandlers"

    let getTeams =
        fun (getTeams : unit -> Team list) (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams"
                
                let teams = getTeams()

                return! json teams next ctx
            }

    let getTeamsHandler : HttpHandler = 
        getTeams TeamRepository.getAll


    let getTeam (getTeamById : int -> Team) (id : int) =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Team: id={id}"
                
                let team = getTeamById(id)

                return! json team next ctx
            }

    let getSingleTeamHandler : (int -> HttpHandler) =
        getTeam TeamRepository.getById