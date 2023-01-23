namespace ftbbl.WebApi.Services

open System


module EloHistoryService =
    open ftbbl.WebApi.Repositories
    open ftbbl.WebApi.Models

    let getTeamEloHistory teamId =
        let team = TeamRepository.getById teamId
        List.append 
            (TeamEloHistoryRepository.getByTeam teamId)
            [{ Id = 0; TeamId = teamId; Elo = team.Elo; Date = DateTime.Now }] 

    let getCoachEloHistory coachId =
        let coach = CoachRepository.getById coachId
        List.append
            (CoachEloHistoryRepository.getByCoach coachId)
            [{ Id = 0; CoachId = coachId; Elo = coach.Elo; Date = DateTime.Now }]