namespace ftbbl.WebApi.Services


module TeamService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models
    open System

    let getAll =
        TeamRepository.getAll

    let getById id =
        TeamRepository.getById id

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

    let getByDiv divId = 
        TeamRepository.getByDiv divId

    let getNotInDiv divId = 
        TeamRepository.getNotInDiv divId
