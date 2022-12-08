namespace ftbbl.WebApi.Services


module CoachService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models
    open System

    let private populateCoachAccollades (accolades : Accolade list) (coach : Coach) : Coach =
        { coach with Accolades = List.filter (fun acc -> acc.CoachId = coach.Id) accolades }

    let getAll() =
        let accolades = AccoladeService.getAll()

        CoachRepository.getAll()
        |> List.map (populateCoachAccollades accolades)

    let getById id =
        let accolades = AccoladeService.getAll()

        CoachRepository.getById id
        |> populateCoachAccollades accolades

    let saveChanges (coach : Coach)= 
        let oldCoach = CoachRepository.getById(coach.Id)
        
        if (oldCoach.Elo <> coach.Elo) 
        then CoachEloHistoryRepository.save({Id = 0; CoachId=oldCoach.Id; Elo = oldCoach.Elo; Date = DateTime.Now})

        CoachRepository.save(coach)

        coach

    let deleteById id =
        CoachRepository.deleteById(id)

    let saveOverId id coach =
        saveChanges { coach with Id = id }

    let saveNew coach =
        let newCoach:Coach = { coach with Elo = 1000 }

        CoachRepository.save newCoach

        newCoach
