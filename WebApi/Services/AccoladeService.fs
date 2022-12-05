namespace ftbbl.WebApi.Services

module AccoladeService =
    open ftbbl.WebApi.Repositories

    let getAll =
        AccoladeRepository.getAll

    let getAllForTeam =
        AccoladeRepository.getAllForTeam

    let getAllForCoach =
        AccoladeRepository.getAllForCoach

    let getAllForCoachExcludeTeam (coachId:int) (teamId:int) =
        AccoladeRepository.getAllForCoach coachId
        |> List.filter (fun accolade -> not accolade.TeamId.HasValue || accolade.TeamId.Value <> teamId)

    let getById id =
        AccoladeRepository.getById id

    let saveChanges accolade= 
        AccoladeRepository.save(accolade)

        accolade

    let deleteById id =
        AccoladeRepository.deleteById(id)

    let saveOverId id accolade =
        saveChanges { accolade with Id = id }

