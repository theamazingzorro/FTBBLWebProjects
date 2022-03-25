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


    let postCoach : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! coach = ctx.BindJsonAsync<Coach>()
                let coach = { coach with Elo = 1000 }

                logger.LogInformation $"Saving Coach: name={coach.Name}"
                
                CoachRepository.save(coach)

                return! json coach next ctx
            }


    let deleteCoach (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Coach: id={id}"
                let res = CoachRepository.deleteById(id)
                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateCoach (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! coach = ctx.BindJsonAsync<Coach>()
                logger.LogInformation $"Updating Coach: id={id}"

                CoachRepository.update { coach with Id = id }

                return! json coach next ctx
            }
