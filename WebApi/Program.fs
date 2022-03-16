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

                    routex "/coach(/?)" >=> CoachApiHandlers.getCoaches
                    routef "/coach/%i" CoachApiHandlers.getCoach
                    routef "/coach/%i/" CoachApiHandlers.getCoach
                ]
                POST >=> choose [
                    routex "/team(/?)" >=> TeamApiHandlers.postTeam

                    routex "/coach(/?)" >=> CoachApiHandlers.postCoach
                ]
                PUT >=> choose [
                    routex "/team(/?)" >=> TeamApiHandlers.updateTeam

                    routex "/coach(/?)" >=> CoachApiHandlers.updateCoach
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

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001",
            "http://localhost:8000",
            "https://localhost:8000")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  ->
        app.UseDeveloperExceptionPage()
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
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