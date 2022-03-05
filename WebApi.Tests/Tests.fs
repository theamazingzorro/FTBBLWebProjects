module ftbbl.WebApi.Tests

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models

open System
open Xunit
open Giraffe
open System.Threading.Tasks
open NSubstitute
open Microsoft.AspNetCore.Http
//open Giraffe.Serialization
open System.IO
open System.Text
open Newtonsoft.Json

let next : HttpFunc = Some >> Task.FromResult

let buildMockContext () =
    let context = Substitute.For<HttpContext>()
    context.RequestServices.GetService(typeof<INegotiationConfig>).Returns(DefaultNegotiationConfig()) |> ignore
    context.RequestServices.GetService(typeof<Json.ISerializer>).Returns(Giraffe.NewtonsoftJson.Serializer(NewtonsoftJson.Serializer.DefaultSettings)) |> ignore
    context.Request.Headers.ReturnsForAnyArgs(new HeaderDictionary()) |> ignore
    context.Response.Body <- new MemoryStream()
    context
    
let getBody (ctx : HttpContext) =
    ctx.Response.Body.Position <- 0L
    use reader = new StreamReader(ctx.Response.Body, System.Text.Encoding.UTF8)
    reader.ReadToEnd()

[<Fact>]
let ``print msg handler prints a message from the body`` () =
    let handler = HttpHandlers.handleGetHello

    let message = { Text = "hello world" }
    let postData = Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(message))
    
    let context = buildMockContext()
    context.Request.Body <- new MemoryStream(postData) 

    task {
        let! response = handler next context
        Assert.True(response.IsSome)
        let context = response.Value
        let body = getBody context
        Assert.Equal("""{"text":"Hello world, from Giraffe!"}""", body)
    }
    
