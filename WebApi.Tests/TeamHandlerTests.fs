module ftbbl.WebApi.Tests.TeamHandlerTest

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models
open ftbbl.WebApi.Tests.ContextTools

open Xunit
open Newtonsoft.Json


[<Fact>]
let ``getTeams returns a list of teams`` () =
    let testTeams: Team list = [
            { Name="Team 1"; Race="Lizardmen"; Coach="Coach 1"; IsActive=true }
            { Name="Team 2"; Race="Necromantic"; Coach="Coach 2"; IsActive=true }
        ]

    let teamRepoMock() = 
        testTeams

    let handler = TeamApiHandlers.getTeams teamRepoMock 
    let context = buildMockContext()

    task {
        let! response = handler next context
        Assert.True(response.IsSome)

        let data = 
            response.Value
            |> getBody
            |> JsonConvert.DeserializeObject<List<Team>>

        Assert.Equal<List<Team>>(testTeams, data)
    }
    
[<Fact>]
let ``getTeam returns a single team with the given id`` () =
    let testTeam = { Name="Team 1"; Race="Lizardmen"; Coach="Coach 1"; IsActive=true }
         
    let teamRepoMock(id: int) = 
        Assert.Equal(1, id)
        testTeam

    let handler = TeamApiHandlers.getTeam 1 teamRepoMock 
    let context = buildMockContext()

    task {
        let! response = handler next context
        Assert.True(response.IsSome)

        let data = 
            response.Value
            |> getBody
            |> JsonConvert.DeserializeObject<Team>

        Assert.Equal(testTeam, data)
    }
