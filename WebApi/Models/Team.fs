﻿namespace ftbbl.WebApi.Models

[<CLIMutable>]
type Team =
    {
        Id : int
        Name : string
        Race: Race
        Coach: string
    }
