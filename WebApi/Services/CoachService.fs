namespace ftbbl.WebApi.Services


module CoachService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models
    open System

    let private populateCoachAccollades (accolades : Accolade list) (coach : Coach) : Coach =
        { coach with Accolades = List.filter (fun acc -> acc.CoachId = coach.Id) accolades }

    let getAll(leagueId) =
        let accolades = AccoladeService.getAll()

        CoachRepository.getAll(leagueId)
        |> List.map (populateCoachAccollades accolades)

    let getById id =
        let accolades = AccoladeService.getAll()

        CoachRepository.getById id
        |> populateCoachAccollades accolades

    let saveChanges (coach : Coach)= 
        let oldCoach = CoachRepository.getById(coach.Id)
        
        if (oldCoach.Elo <> coach.Elo) 
        then CoachEloHistoryRepository.save({Id = 0; CoachId=oldCoach.Id; Elo = oldCoach.Elo; Date = DateTime.Now})

        CoachRepository.save({coach with LeagueId = oldCoach.LeagueId})

        coach

    let deleteById id =
        CoachRepository.deleteById(id)

    let saveOverId id coach league =
        saveChanges { coach with Id = id; LeagueId = league }

    let saveNew coach league=
        let newCoach:Coach = { coach with Elo = 1000; LeagueId = league }

        CoachRepository.save newCoach

        newCoach
