namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module TeamApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.TeamApiHandlers"

    let getTeams : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Teams"
                
                let teams = TeamRepository.getAll()

                return! json teams next ctx
            }


    let getTeam (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Team: id={id}"
                
                let team = TeamRepository.getById id

                return! json team next ctx
            }


    let postTeam : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! team = ctx.BindJsonAsync<Team>()
                let team = { team with Elo = 1000 }

                logger.LogInformation $"Saving Team: name={team.Name}"
                
                TeamRepository.save(team)

                return! json team next ctx
            }


    let deleteTeam (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Team: id={id}"
                let res = TeamRepository.deleteById(id)

                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateTeam (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! team = ctx.BindJsonAsync<Team>()
                logger.LogInformation $"Updating Team: id={id}"

                let oldTeam = TeamRepository.getById(id)
                if (oldTeam.Elo <> team.Elo) 
                then TeamEloHistoryRepository.save({Id = 0; TeamId=id; Elo = oldTeam.Elo; Date = DateTime.Now})

                TeamRepository.update { team with Id = id }

                return! json team next ctx
            }