namespace ftbbl.WebApi.Services


module TeamService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Services
    open ftbbl.WebApi.Models
    open System

    let private populateCoachAccollades (teamId : int) (coach : Coach) : Coach =
        if coach.AccoladeCount = 0
        then { coach with Accolades = [] }
        else { coach with Accolades = AccoladeService.getAllForCoachExcludeTeam teamId coach.Id }

    let private populateAccolades (team : Team) : Team =
        if team.AccoladeCount = 0 
        then 
            { team with 
                Accolades = []; 
                Coach = populateCoachAccollades team.Id team.Coach 
            }
        else 
            { team with 
                Accolades = AccoladeService.getAllForTeam team.Id; 
                Coach = populateCoachAccollades team.Id team.Coach 
            }

    let getAll() =
        TeamRepository.getAll()
        |> List.map populateAccolades

    let getFree() = 
        TeamRepository.getFree()
        |> List.map populateAccolades

    let getByDiv divId = 
        TeamRepository.getByDiv divId
        |> List.map populateAccolades

    let getNotInDiv divId = 
        TeamRepository.getNotInDiv divId
        |> List.map populateAccolades

    let getById id =
        TeamRepository.getById id
        |> populateAccolades

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
