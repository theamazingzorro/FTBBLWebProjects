namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module CoachHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.CoachApiHandlers"

    let getCoaches : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Coaches"
                
                let result = CoachService.getAll()

                return! json result next ctx
            }


    let getCoach (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Coach: id={id}"
                
                let result = CoachService.getById id

                return! json result next ctx
            }


    let postCoach : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! coach = ctx.BindJsonAsync<Coach>()

                logger.LogInformation $"Saving Coach: name={coach.Name}"
                
                let result = CoachService.saveNew coach

                return! json result next ctx
            }


    let deleteCoach (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Coach: id={id}"
                let res = CoachService.deleteById id
                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateCoach (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! coach = ctx.BindJsonAsync<Coach>()
                logger.LogInformation $"Updating Coach: id={id}"

                let result = CoachService.saveOverId id coach

                return! json result next ctx
            }
