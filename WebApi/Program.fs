module ftbbl.WebApi.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe

open ftbbl.WebApi.Handlers

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [
        subRoute "/api"
            ( choose [
                GET >=> choose [
                    routex "/team(/?)" >=> TeamApiHandlers.getTeams
                    routef "/team/%i" TeamApiHandlers.getTeam
                    routef "/team/%i/" TeamApiHandlers.getTeam

                    routex "/race(/?)" >=> RaceApiHandlers.getRaces
                    routef "/race/%i" RaceApiHandlers.getRace
                    routef "/race/%i/" RaceApiHandlers.getRace

                    routex "/coach(/?)" >=> CoachApiHandlers.getCoaches
                    routef "/coach/%i" CoachApiHandlers.getCoach
                    routef "/coach/%i/" CoachApiHandlers.getCoach

                    routex "/div(/?)" >=> DivisionApiHandlers.getDivisions
                    routef "/div/%i" DivisionApiHandlers.getDivision
                    routef "/div/%i/" DivisionApiHandlers.getDivision
                ]
                POST >=> choose [
                    routex "/team(/?)" >=> TeamApiHandlers.postTeam

                    routex "/coach(/?)" >=> CoachApiHandlers.postCoach

                    routex "/div(/?)" >=> DivisionApiHandlers.postDivision
                ]
                PUT >=> choose [
                    routef "/team/%i" TeamApiHandlers.updateTeam
                    routef "/team/%i/" TeamApiHandlers.updateTeam

                    routef "/coach/%i" CoachApiHandlers.updateCoach
                    routef "/coach/%i/" CoachApiHandlers.updateCoach

                    routef "/div/%i" DivisionApiHandlers.updateDivision
                    routef "/div/%i/" DivisionApiHandlers.updateDivision
                ]
                DELETE >=> choose [
                    routef "/team/%i" TeamApiHandlers.deleteTeam
                    routef "/team/%i/" TeamApiHandlers.deleteTeam

                    routef "/coach/%i" CoachApiHandlers.deleteCoach
                    routef "/coach/%i/" CoachApiHandlers.deleteCoach
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
            "https://ftbbl-elo.github.io/"
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
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()
        .AddLogging()
        .AddGiraffe() 
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