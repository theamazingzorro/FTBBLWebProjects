namespace ftbbl.Handlers

open Microsoft.Extensions.Logging

module HttpHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open ftbbl.Models

    let handleGetHello =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let response = {
                    Text = "Hello world, from Giraffe!"
                }
                return! json response next ctx
            }

    let printMsg : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! msg = ctx.BindJsonAsync<Message>()

                let logger = ctx.GetLogger("fttbl.Handlers.HttpHandlers")
                logger.LogInformation($"From the context: {msg.Text}")
                
                return! text msg.Text next ctx
            }