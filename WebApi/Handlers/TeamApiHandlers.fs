﻿namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module TeamApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Repositories

    let logName = "fttbl.Handlers.TeamApiHandlers"

    let getTeams =
        fun (getTeams : unit -> Team list) (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = ctx.GetLogger logName
                logger.LogInformation $"Getting Teams"
                
                let teams = getTeams()

                return! json teams next ctx
            }

    let getTeamsHandler : HttpHandler = 
        getTeams TeamRepository.getAll