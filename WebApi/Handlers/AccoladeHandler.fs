namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging
open System

module AccoladeHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.AccoladeHandler"

    let getAccolades : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Accolades"
                
                let result = AccoladeService.getAll()

                return! json result next ctx
            }

    let getAccolade (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Accolade with id={id}"
                
                let result = AccoladeService.getById id

                return! json result next ctx
            }


    let postAccolade : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! accolade = ctx.BindJsonAsync<Accolade>()

                logger.LogInformation $"Saving New Accolade: coachId={accolade.CoachId}, name={accolade.Name}"
                
                let result = AccoladeService.saveChanges accolade

                return! json result next ctx
            }


    let deleteAccolade (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Accolade: id={id}"
                let res = AccoladeService.deleteById id

                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateAccolade (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! accolade = ctx.BindJsonAsync<Accolade>()
                logger.LogInformation $"Updating Accolade: id={id}"

                let result = AccoladeService.saveOverId id accolade

                return! json result next ctx
            }
