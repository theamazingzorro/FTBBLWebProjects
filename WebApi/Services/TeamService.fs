namespace ftbbl.WebApi.Services


module TeamService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Services
    open ftbbl.WebApi.Models

    open System
    open Linq.NullableOperators

    let private populateAccolades (accolades : Accolade list) (team : Team) : Team =
        { team with 
                Accolades = List.filter (fun acc -> acc.TeamId ?= team.Id) accolades; 
                Coach = { team.Coach with Accolades = List.filter (fun acc -> acc.CoachId = team.Coach.Id) accolades }
        }

    let getAll() =
        let accolades = AccoladeService.getAll()

        TeamRepository.getAll()
        |> List.map (populateAccolades accolades)

    let getFree() = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getFree()
        |> List.map (populateAccolades accolades)

    let getByDiv divId = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getByDiv divId
        |> List.map (populateAccolades accolades)

    let getNotInDiv divId = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getNotInDiv divId
        |> List.map (populateAccolades accolades)

    let getById id =
        let accolades = AccoladeService.getAll()

        TeamRepository.getById id
        |> populateAccolades accolades

    let saveChanges (team : Team)= 
        let oldTeam = TeamRepository.getById(team.Id)
        
        if (oldTeam.Elo <> team.Elo) 
        then TeamEloHistoryRepository.save({Id = 0; TeamId=oldTeam.Id; Elo = oldTeam.Elo; Date = DateTime.Now})

        TeamRepository.save(team)

        team

    let deleteById id =
        TeamRepository.deleteById(id)

    let saveOverId id team =
        saveChanges { team with Id = id }

    let saveNew team =
        let newTeam:Team = { team with Elo = 1000 }

        TeamRepository.save newTeam

        newTeam

    let updateDiv teamId divId = 
        TeamRepository.updateDiv teamId divId
