namespace ftbbl.WebApi.Handlers

open Microsoft.Extensions.Logging


module SecurityHandler =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.WebApi.Models
    open ftbbl.WebApi.Services

    let private getLogger (ctx : HttpContext) = 
        ctx.GetLogger "fttbl.Handlers.SecurityHandler"

    let signIn : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = getLogger ctx

                let! user = ctx.BindJsonAsync<User>()

                logger.LogInformation $"Sign in attempt for {user.Username}"
                
                let token = Auth.getTokenFor(user.Username, user.Password)

                if (token <> "") then logger.LogInformation $"Sign in success"
                else logger.LogInformation $"Sign in failed"

                return! text token next ctx
            }