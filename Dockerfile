FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build

ARG BUILD_CONFIGURATION=Prod
ENV DOTNET_USE_POLLING_FILE_WATCHER=true  
ENV ASPNETCORE_URLS=http://+:80

WORKDIR /app

COPY . .
RUN dotnet restore
RUN dotnet publish -c release -o /app --no-self-contained --no-restore

# create final image
FROM mcr.microsoft.com/dotnet/sdk:6.0

ARG BUILD_CONFIGURATION=Prod
ENV DOTNET_USE_POLLING_FILE_WATCHER=true  
ENV ASPNETCORE_URLS=http://+:80
EXPOSE 80

WORKDIR /app
COPY --from=build /app ./

ENTRYPOINT ["./WebApi.App"]