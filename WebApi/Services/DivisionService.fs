namespace ftbbl.WebApi.Services

module DivisionService =
    open ftbbl.WebApi.Repositories

    let getAll leagueId =
        DivisionRepository.getAll leagueId

    let getAllForTeam teamId = 
        DivisionRepository.getAllForTeam teamId

    let getById id =
        DivisionRepository.getById id

    let saveChanges division league= 
        DivisionRepository.save({division with LeagueId = league})

        division

    let deleteById id =
        DivisionRepository.deleteById(id)

    let saveOverId id division league=
        saveChanges { division with Id = id } league

    let closeDiv id =
        DivisionRepository.closeDiv id
