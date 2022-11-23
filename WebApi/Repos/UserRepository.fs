namespace ftbbl.WebApi.Repositories

module UserRepository =

    open ftbbl.WebApi.Models

    open Microsoft.AspNetCore.Builder
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]

    let getAll() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<User>("""
                SELECT * FROM User
                """)
            |> List.ofSeq


    let getByUsername (username : string) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.SingleOrDefault<User>("""
            SELECT * FROM User
            WHERE User.username=@0""", username)


    let save (user : User) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Save<User>(user)
