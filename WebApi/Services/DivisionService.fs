namespace ftbbl.WebApi.Services

module DivisionService =
    open ftbbl.WebApi.Repositories

    let getAll =
        DivisionRepository.getAll

    let getById id =
        DivisionRepository.getById id

    let saveChanges division= 
        DivisionRepository.save(division)

        division

    let deleteById id =
        DivisionRepository.deleteById(id)

    let saveOverId id division =
        saveChanges { division with Id = id }

    let closeDiv id =
        DivisionRepository.closeDiv id
