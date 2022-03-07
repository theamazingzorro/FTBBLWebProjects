module ftbbl.WebApi.Tests.TeamHandlerTest

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models
open ftbbl.WebApi.Tests.ContextTools

open Xunit
open Newtonsoft.Json


[<Fact>]
let ``getTeams returns a list of teams`` () =
    let testTeams: Team list = [
            { Name="Team 1"; Race=6; Coach="Coach 1" }
            { Name="Team 2"; Race=17; Coach="Coach 2" }
        ]

    let getAllTeamsMock() = 
        testTeams

    let handler = TeamApiHandlers.getTeams getAllTeamsMock 
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
    let testTeam = { Name="Team 1"; Race=6; Coach="Coach 1" }
         
    let getTeamByIdMock(id: int) = 
        Assert.Equal(1, id)
        testTeam

    let handler = TeamApiHandlers.getTeam 1 getTeamByIdMock 
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
