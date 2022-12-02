namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module DivisionHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.DivisionApiHandlers"

    let getDivisions : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Divisions"
                
                let result = DivisionService.getAll()

                return! json result next ctx
            }


    let getDivision (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Division: id={id}"
                
                let result = DivisionService.getById id

                return! json result next ctx
            }


    let postDivision : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! division = ctx.BindJsonAsync<Division>()

                logger.LogInformation $"Saving Division: name={division.Name}"
                
                let result = DivisionService.saveChanges division

                return! json result next ctx
            }


    let deleteDivision (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Division: id={id}"
                let result = DivisionService.deleteById id

                logger.LogInformation $"{result} rows effected."

                return! json {| Deleted = result > 0 |} next ctx
            }


    let updateDivision (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! division = ctx.BindJsonAsync<Division>()
                logger.LogInformation $"Updating Division: id={id}"

                let result = DivisionService.saveOverId id division

                return! json result next ctx
            }