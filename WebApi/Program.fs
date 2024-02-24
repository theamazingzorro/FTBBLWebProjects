module ftbbl.WebApi.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Authentication.JwtBearer
open Giraffe

open ftbbl.WebApi.Handlers
open ftbbl.WebApi.Services
open Microsoft.IdentityModel.Tokens
open System.Text

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [
        subRoute "/api"
            ( choose [
                GET >=> choose [
                    routex "/team(/?)" >=> TeamHandler.getTeams
                    routef "/team/%i" TeamHandler.getTeam
                    routef "/team/%i/" TeamHandler.getTeam
                    routex "/team/free(/?)" >=> TeamHandler.getFreeTeams
                    routef "/team/bydiv/%i" TeamHandler.getTeamsByDiv
                    routef "/team/bydiv/%i/" TeamHandler.getTeamsByDiv
                    routef "/team/notindiv/%i" TeamHandler.getTeamsNotInDiv
                    routef "/team/notindiv/%i/" TeamHandler.getTeamsNotInDiv
                    routef "/team/bycoach/%i" TeamHandler.getTeamsByCoach
                    routef "/team/bycoach/%i/" TeamHandler.getTeamsByCoach

                    routex "/race(/?)" >=> RaceHandler.getRaces
                    routef "/race/%i" RaceHandler.getRace
                    routef "/race/%i/" RaceHandler.getRace

                    routex "/coach(/?)" >=> CoachHandler.getCoaches
                    routef "/coach/%i" CoachHandler.getCoach
                    routef "/coach/%i/" CoachHandler.getCoach

                    routex "/div(/?)" >=> DivisionHandler.getDivisions
                    routef "/div/%i" DivisionHandler.getDivision
                    routef "/div/%i/" DivisionHandler.getDivision

                    routex "/game(/?)" >=> GameHandler.getGames
                    routef "/game/%i" GameHandler.getGame
                    routef "/game/%i/" GameHandler.getGame
                    routef "/game/bydiv/%i" GameHandler.getGamesByDiv
                    routef "/game/bydiv/%i/" GameHandler.getGamesByDiv
                    routef "/game/teams/%i/%i" GameHandler.getGamesByTeams
                    routef "/game/teams/%i/%i/" GameHandler.getGamesByTeams
                    routef "/game/coaches/%i/%i" GameHandler.getGamesByCoaches
                    routef "/game/coaches/%i/%i/" GameHandler.getGamesByCoaches

                    routef "/standings/%i" StandingHandler.getDivStandings
                    routef "/standings/%i/" StandingHandler.getDivStandings
                    routef "/standings/%i/%i" StandingHandler.getStanding
                    routef "/standings/%i/%i/" StandingHandler.getStanding
                    routef "/standings/team/%i" StandingHandler.getAllTeamStandings
                    routef "/standings/team/%i/" StandingHandler.getAllTeamStandings

                    routex "/accolade(/?)" >=> AccoladeHandler.getAccolades
                    routef "/accolade/%i" AccoladeHandler.getAccolade
                    routef "/accolade/%i/" AccoladeHandler.getAccolade

                    routef "/history/team/%i" EloHistoryHandler.getTeamHistory
                    routef "/history/team/%i/" EloHistoryHandler.getTeamHistory
                    routef "/history/coach/%i" EloHistoryHandler.getCoachHistory
                    routef "/history/coach/%i/" EloHistoryHandler.getCoachHistory
                ]
                POST >=> choose [
                    routex "/signin(/?)" >=> SecurityHandler.signIn

                    Auth.enticate >=> choose [
                        routex "/team(/?)" >=> TeamHandler.postTeam

                        routex "/coach(/?)" >=> CoachHandler.postCoach

                        routex "/div(/?)" >=> DivisionHandler.postDivision

                        routex "/game(/?)" >=> GameHandler.postGame

                        routex "/accolade(/?)" >=> AccoladeHandler.postAccolade

                        routef "/team/updatediv/%i/%i" TeamHandler.updateDiv
                        routef "/team/updatediv/%i/%i/" TeamHandler.updateDiv

                        routef "/div/close/%i" DivisionHandler.closeDiv
                        routef "/div/close/%i/" DivisionHandler.closeDiv
                    ]
                ]
                PUT >=> Auth.enticate >=> choose [
                    routef "/team/%i" TeamHandler.updateTeam
                    routef "/team/%i/" TeamHandler.updateTeam

                    routef "/coach/%i" CoachHandler.updateCoach
                    routef "/coach/%i/" CoachHandler.updateCoach

                    routef "/div/%i" DivisionHandler.updateDivision
                    routef "/div/%i/" DivisionHandler.updateDivision

                    routef "/game/%i" GameHandler.updateGame
                    routef "/game/%i/" GameHandler.updateGame

                    routef "/accolade/%i" AccoladeHandler.updateAccolade
                    routef "/accolade/%i/" AccoladeHandler.updateAccolade
                ]
                DELETE >=> Auth.enticate >=> choose [
                    routef "/team/%i" TeamHandler.deleteTeam
                    routef "/team/%i/" TeamHandler.deleteTeam

                    routef "/coach/%i" CoachHandler.deleteCoach
                    routef "/coach/%i/" CoachHandler.deleteCoach

                    routef "/div/%i" DivisionHandler.deleteDivision
                    routef "/div/%i/" DivisionHandler.deleteDivision

                    routef "/game/%i" GameHandler.deleteGame
                    routef "/game/%i/" GameHandler.deleteGame

                    routef "/accolade/%i" AccoladeHandler.deleteAccolade
                    routef "/accolade/%i/" AccoladeHandler.deleteAccolade
                ]
            ])
        setStatusCode 404 >=> text "Not Found" 
    ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureDevCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001",
            "http://localhost:8000",
            "https://localhost:8000",
            "http://localhost:8080",
            "https://localhost:8080",
            "http://127.0.0.1:8080"
        )
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureProdCors (builder : CorsPolicyBuilder) =
    builder
       .WithOrigins(
            "https://ftbbl-elo.github.io",
            "https://top-scumlord.github.io"
       )
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    ( match env.IsDevelopment() with
    | true  ->
        app .UseDeveloperExceptionPage()
            .UseCors(configureDevCors)
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection()
            .UseCors(configureProdCors)
    )
        .UseStaticFiles()
        .UseAuthentication()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()
        .AddLogging()
        .AddGiraffe() 
        .AddAuthentication(fun opt ->
            opt.DefaultAuthenticateScheme <- JwtBearerDefaults.AuthenticationScheme
        )
        .AddJwtBearer(fun opt ->
            opt.TokenValidationParameters <- TokenValidationParameters(
                IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(Auth.key)),
                ValidateIssuer = true,
                ValidIssuer = Auth.site,
                ValidateAudience = true,
                ValidAudience = Auth.site
            )
        )
        |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddConsole()
           .AddDebug() 
           |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0