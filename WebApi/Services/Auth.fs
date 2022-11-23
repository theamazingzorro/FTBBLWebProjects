namespace ftbbl.WebApi.Services

open Giraffe
open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.AspNetCore.Builder
open ftbbl.WebApi.Models

module Auth =

    open System
    open System.Text
    open System.IdentityModel.Tokens.Jwt 
    open System.Security.Claims
    open Microsoft.IdentityModel.Tokens
    open ftbbl.WebApi.Repositories

    let key = WebApplication.CreateBuilder().Configuration["JWTKey"]
    let site : string = "http://ftbbl-elo.github.io/"
    let lifespan = 24 * 60

    let getTokenFor(username : String, password : String) : string =
        let user = UserRepository.getByUsername(username)

        if (user :> obj = null || user.Password <> password) then ""
        else

        let claims =[|
            if (user.IsAdmin) then new Claim(ClaimTypes.Role, "Admin")
        |]

        let securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(key))
        let creds = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256)

        let token = new JwtSecurityToken(
            site, 
            site, 
            claims,
            System.Nullable(), 
            DateTime.Now.AddMinutes(lifespan), 
            creds)

        let handler = new JwtSecurityTokenHandler()

        handler.WriteToken(token)


    let enticate : HttpHandler =
        requiresAuthentication (challenge JwtBearerDefaults.AuthenticationScheme >=> text "Authorization required.")

    let requireAdminRole : HttpHandler = 
        requiresRole "Admin" (RequestErrors.FORBIDDEN  "Permission denied. You must be an admin.")
