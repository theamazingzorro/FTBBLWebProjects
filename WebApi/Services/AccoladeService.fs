namespace ftbbl.WebApi.Services

module AccoladeService =
    open ftbbl.WebApi.Repositories

    let getAll =
        AccoladeRepository.getAll

    let getById id =
        AccoladeRepository.getById id

    let saveChanges accolade= 
        AccoladeRepository.save(accolade)

        accolade

    let deleteById id =
        AccoladeRepository.deleteById(id)

    let saveOverId id accolade =
        saveChanges { accolade with Id = id }

