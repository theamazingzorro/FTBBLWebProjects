module ftbbl.WebApi.Tests.TeamHandlerTest

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models
open ftbbl.WebApi.Tests.ContextTools

open Xunit
open System.Text
open Newtonsoft.Json
open System.IO


[<Fact>]
let ``getTeams returns a list of teams`` () =
    let handler = TeamApiHandlers.getTeams
    
    let context = buildMockContext()

    task {
        let! response = handler next context
        Assert.True(response.IsSome)
        let context = response.Value
        let body = getBody context

        let data = JsonConvert.DeserializeObject<List<Team>>(body)

        let expectedData: List<Team> = [
            {Name="The Government"; Race="Lizardmen"; Coach="Theamazingzorro"; IsActive=true}
            {Name="Scooby Snacks"; Race="Necromantic"; Coach="Danean"; IsActive=true}
        ]

        Assert.Equal<List<Team>>(data, expectedData)
    }
    
