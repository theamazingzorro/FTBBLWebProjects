namespace ftbbl.WebApi.Services

open Giraffe
open Microsoft.AspNetCore.Authentication.JwtBearer

module Auth =

    open System
    open System.Text
    open System.IdentityModel.Tokens.Jwt 
    open System.Security.Claims
    open Microsoft.IdentityModel.Tokens

    let key = "pleaseReplaceWithYourSecretKeyRetrievedFromSomeSecureLocation"
    let site : string = "http://ftbbl-elo.github.io/"
    let lifespan = 24 * 60

    let getToken() : string =
        let claims =[|
            new Claim(ClaimTypes.Role, "Admin")
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
        requiresAuthentication (challenge JwtBearerDefaults.AuthenticationScheme >=> text "Authentication required.")
