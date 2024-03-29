﻿namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module TeamHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.TeamApiHandlers"

    let getTeams : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams"
                
                let league = ctx.Request.Headers["league"].ToString() |> int
                let result = TeamService.getAll(league)

                return! json result next ctx
            }


    let getFreeTeams : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams not in any Division"
                
                let league = ctx.Request.Headers["league"].ToString() |> int
                let result = TeamService.getFree(league)

                return! json result next ctx
            }


    let getTeamsByDiv (divId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams for division: div id={divId}"
                
                let result = TeamService.getByDiv divId

                return! json result next ctx
            }


    let getTeamsByCoach (coachId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams for coach: id={coachId}"
                
                let result = TeamService.getByCoach coachId

                return! json result next ctx
            }


    let getTeamsNotInDiv (divId : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams for all other divs: div id={divId}"
                
                let league = ctx.Request.Headers["league"].ToString() |> int
                let result = TeamService.getNotInDiv league divId

                return! json result next ctx
            }


    let getTeam (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Team: id={id}"
                
                let result = TeamService.getById id

                return! json result next ctx
            }


    let postTeam : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! team = ctx.BindJsonAsync<Team>()

                logger.LogInformation $"Saving Team: name={team.Name}"
                
                let league = ctx.Request.Headers["league"].ToString() |> int
                let result = TeamService.saveNew team league

                return! json result next ctx
            }


    let deleteTeam (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Team: id={id}"
                let res = TeamService.deleteById id

                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateTeam (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! team = ctx.BindJsonAsync<Team>()
                logger.LogInformation $"Updating Team: id={id}"

                let league = ctx.Request.Headers["league"].ToString() |> int
                let result = TeamService.saveOverId id team league

                return! json result next ctx
            }

    let updateDiv (teamId, divId) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! team = ctx.BindJsonAsync<Team>()
                logger.LogInformation $"Giving Team new Division: teamId={teamId}, divId={divId}"

                let result = TeamService.updateDiv teamId divId

                return! json result next ctx
            }