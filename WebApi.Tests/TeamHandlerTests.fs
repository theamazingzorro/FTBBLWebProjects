module ftbbl.WebApi.Tests.TeamHandlerTest

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models
open ftbbl.WebApi.Tests.ContextTools

open Xunit
open Newtonsoft.Json


[<Fact>]
let ``getTeams returns a list of teams`` () =
    let expectedData: Team list = [
            { Name="Team 1"; Race="Lizardmen"; Coach="Theamazingzorro"; IsActive=true }
            { Name="Team 2"; Race="Necromantic"; Coach="Danean"; IsActive=true }
        ]

    let teamRepoMock() = 
        expectedData

    let handler = TeamApiHandlers.getTeams teamRepoMock 
    
    let context = buildMockContext()

    task {
        let! response = handler next context
        Assert.True(response.IsSome)

        let body = getBody (response.Value)
            
        let data = JsonConvert.DeserializeObject<List<Team>>(body)

        Assert.Equal<List<Team>>(expectedData, data)
    }
    
[<Fact>]
let ``getTeam returns a single team with the given id`` () =
    let testTeam = { Name="Team 1"; Race="Lizardmen"; Coach="Theamazingzorro"; IsActive=true }
            

    let teamRepoMock(id: int) = 
        Assert.Equal(1, id)
        testTeam

    let handler = TeamApiHandlers.getTeam 1 teamRepoMock 
    
    let context = buildMockContext()

    task {
        let! response = handler next context
        Assert.True(response.IsSome)

        let body = getBody (response.Value)
            
        let data = JsonConvert.DeserializeObject<Team>(body)

        Assert.Equal(testTeam, data)
    }
