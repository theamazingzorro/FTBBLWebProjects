namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging

module DivisionApiHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Repositories

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.DivisionApiHandlers"

    let getDivisions : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Divisions"
                
                let divisions = DivisionRepository.getAll()

                return! json divisions next ctx
            }


    let getDivision (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                logger.LogInformation $"Getting Division: id={id}"
                
                let division = DivisionRepository.getById id

                return! json division next ctx
            }


    let postDivision : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! division = ctx.BindJsonAsync<Division>()

                logger.LogInformation $"Saving Division: name={division.Name}"
                
                DivisionRepository.save(division)

                return! json division next ctx
            }


    let deleteDivision (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx
                
                logger.LogInformation $"Deleting Division: id={id}"
                let res = DivisionRepository.deleteById(id)

                logger.LogInformation $"{res} rows effected."

                return! json {| Deleted = res > 0 |} next ctx
            }


    let updateDivision (id : int) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! division = ctx.BindJsonAsync<Division>()
                logger.LogInformation $"Updating Division: id={id}"

                DivisionRepository.save { division with Id = id }

                return! json division next ctx
            }