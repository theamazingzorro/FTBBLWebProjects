# FTBBL Record Keeper
The ELO calculating and record keeping web app for the Flaming Taurus Bloodbowl League.

Developed fully in the Functional Programming paradigm, using F# and giraffe for the backend API and Elm for frontend.


[Check it out here!](https://ftbbl-elo.github.io/)


## Build Instructions
```
docker build -t web-api .
docker run -p 80:80 web-api
```