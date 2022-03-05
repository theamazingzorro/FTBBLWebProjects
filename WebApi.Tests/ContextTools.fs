namespace ftbbl.WebApi.Tests

open Microsoft.Extensions.Logging

module ContextTools = 

    open System
    open Giraffe
    open System.Threading.Tasks
    open NSubstitute
    open Microsoft.AspNetCore.Http
    open System.IO
    open System.Text

    let next : HttpFunc = Some >> Task.FromResult

    let buildMockContext () =
        let context = Substitute.For<HttpContext>()
        context.RequestServices.GetService(typeof<INegotiationConfig>).Returns(DefaultNegotiationConfig()) |> ignore
        context.RequestServices.GetService(typeof<ILoggerFactory>).Returns(new LoggerFactory()) |> ignore
        context.RequestServices.GetService(typeof<Json.ISerializer>).Returns(Giraffe.NewtonsoftJson.Serializer(NewtonsoftJson.Serializer.DefaultSettings)) |> ignore
        context.Request.Headers.ReturnsForAnyArgs(new HeaderDictionary()) |> ignore
        context.Response.Body <- new MemoryStream()
        context
    
    let getBody (ctx : HttpContext) =
        ctx.Response.Body.Position <- 0L
        use reader = new StreamReader(ctx.Response.Body, System.Text.Encoding.UTF8)
        reader.ReadToEnd()