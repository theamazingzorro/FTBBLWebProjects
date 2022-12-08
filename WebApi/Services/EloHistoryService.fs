namespace ftbbl.WebApi.Services


module EloHistoryService =
    open ftbbl.WebApi.Repositories


    let getTeamEloHistory teamId =
        TeamEloHistoryRepository.getByTeam teamId

    let getCoachEloHistory coachId =
        CoachEloHistoryRepository.getByCoach coachId
