namespace ftbbl.WebApi.Services

open System


module EloHistoryService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models

    let getTeamEloHistory teamId =
        let team = TeamRepository.getById teamId
        let history = TeamEloHistoryRepository.getByTeam teamId
        let weekAfterFinal = (history.Item (history.Length - 1)).Date.AddDays(7)
        let clampedFinal = if (weekAfterFinal > DateTime.Now) then DateTime.Now else weekAfterFinal
        
        List.append history
            [{ Id = 0; TeamId = teamId; Elo = team.Elo; Date = clampedFinal }] 

    let getCoachEloHistory coachId =
        let coach = CoachRepository.getById coachId
        let history = CoachEloHistoryRepository.getByCoach coachId
        let weekAfterFinal = (history.Item (history.Length - 1)).Date.AddDays(7)
        let clampedFinal = if (weekAfterFinal > DateTime.Now) then DateTime.Now else weekAfterFinal
        
        List.append history
            [{ Id = 0; CoachId = coachId; Elo = coach.Elo; Date = clampedFinal }]