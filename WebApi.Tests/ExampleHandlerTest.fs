module ftbbl.WebApi.Tests.ExampleHandlerTest

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models
open ftbbl.WebApi.Tests.ContextTools

open Xunit
open System.Text
open Newtonsoft.Json
open System.IO


[<Fact>]
let ``print msg handler prints a message from the body`` () =
    let handler = HttpHandlers.printMsg

    let message = { Text = "hello world" }
    let postData = Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(message))
    
    let context = buildMockContext()
    context.Request.Body <- new MemoryStream(postData) 

    task {
        let! response = handler next context
        Assert.True(response.IsSome)
        let context = response.Value
        let body = getBody context
        Assert.Equal("""hello world""", body)
    }
    
