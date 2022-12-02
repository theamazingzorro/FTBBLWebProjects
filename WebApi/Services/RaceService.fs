namespace ftbbl.WebApi.Services

module RaceService =
    open ftbbl.WebApi.Repositories

    let getAll =
        RaceRepository.getAll

    let getById id =
        RaceRepository.getById id

