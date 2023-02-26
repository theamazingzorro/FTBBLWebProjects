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

    let getAll(leagueId) =
        let accolades = AccoladeService.getAll()

        TeamRepository.getAll(leagueId)
        |> List.map (populateAccolades accolades)

    let getFree(leagueId) = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getFree(leagueId)
        |> List.map (populateAccolades accolades)

    let getByCoach coachId = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getByCoach coachId
        |> List.map (populateAccolades accolades)

    let getByDiv divId = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getByDiv divId
        |> List.map (populateAccolades accolades)

    let getNotInDiv leagueId divId = 
        let accolades = AccoladeService.getAll()

        TeamRepository.getNotInDiv(leagueId, divId)
        |> List.map (populateAccolades accolades)

    let getById id =
        let accolades = AccoladeService.getAll()

        TeamRepository.getById id
        |> populateAccolades accolades

    let saveChanges (team : Team)= 
        let oldTeam = TeamRepository.getById(team.Id)
        
        if (oldTeam.Elo <> team.Elo) 
        then TeamEloHistoryRepository.save({Id = 0; TeamId=oldTeam.Id; Elo = oldTeam.Elo; Date = DateTime.Now})

        TeamRepository.save({team with LeagueId = oldTeam.LeagueId})

        team

    let deleteById id =
        TeamRepository.deleteById(id)

    let saveOverId id team league=
        saveChanges { team with Id = id; LeagueId = league }

    let saveNew team league =
        let newTeam:Team = { team with Elo = 1000; LeagueId = league }

        TeamRepository.save newTeam

        newTeam

    let updateDiv teamId divId = 
        TeamRepository.updateDiv teamId divId
