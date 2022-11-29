namespace ftbbl.WebApi.Services


module CoachService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models
    open System

    let getAll =
        CoachRepository.getAll

    let getById id =
        CoachRepository.getById id

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
