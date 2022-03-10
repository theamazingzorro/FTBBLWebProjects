module ftbbl.WebApi.Tests.TeamHandlerTest

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Models.Team
open ftbbl.WebApi.Tests.ContextTools

open Xunit
open Newtonsoft.Json


[<Fact>]
let ``getTeams returns a list of teams`` () =
    let testTeams: Team list = [
            { Id = 1; Name="Team 1"; Race={Id=7; Name="Lizardmen"}; Coach={Id=1; Name="Theamazingzorro"; Elo=1000}; Elo=1000 }
            { Id = 2; Name="Team 2"; Race={Id=17; Name="Necromantic"}; Coach={Id=1; Name="Danean"; Elo=1000}; Elo=1000 }
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
    let testTeam = { Id = 1; Name="Team 1"; Race={Id=7; Name="Lizardmen"}; Coach={Id=1; Name="Theamazingzorro"; Elo=1000}; Elo=1000 }
         
    let getTeamByIdMock (id: int) = 
        Assert.Equal(1, id)
        testTeam

    let handler = TeamApiHandlers.getTeam getTeamByIdMock 1
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
