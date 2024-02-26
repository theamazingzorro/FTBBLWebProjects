module Custom.Html exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)



-- Topbar --


navBar : List (Attribute msg) -> List (Html msg) -> Html msg
navBar attributes children =
    div [ class "w3-top" ]
        [ div (class "w3-bar w3-black w3-card" :: attributes)
            children
        ]


menuIcon : List (Attribute msg) -> Html msg
menuIcon attributes =
    a
        ([ class "w3-bar-item w3-button w3-padding-large w3-hide-medium w3-hide-large w3-right"
         , href "#"
         ]
            ++ attributes
        )
        [ i [ class "fa fa-bars" ] [] ]


importantNavButton : List (Attribute msg) -> List (Html msg) -> Html msg
importantNavButton attributes children =
    a
        ([ class "w3-bar-item w3-button w3-padding-large"
         , href "#"
         ]
            ++ attributes
        )
        children


navButton : List (Attribute msg) -> List (Html msg) -> Html msg
navButton attributes children =
    a
        ([ class "w3-bar-item w3-button w3-padding-large w3-hide-small"
         , href "#"
         ]
            ++ attributes
        )
        children


navDropdown : List (Attribute msg) -> List (Html msg) -> Html msg
navDropdown attributes children =
    div (class "w3-dropdown-hover w3-hide-small" :: attributes)
        children


navDropdownTitle : List (Attribute msg) -> List (Html msg) -> Html msg
navDropdownTitle attributes children =
    button (class "w3-padding-large w3-button" :: attributes)
        (children ++ [ i [ class "fa fa-caret-down" ] [] ])


navDropdownContent : List (Attribute msg) -> List (Html msg) -> Html msg
navDropdownContent attributes children =
    div (class "w3-dropdown-content w3-bar-block w3-card-4" :: attributes)
        children


navDropdownLink : List (Attribute msg) -> List (Html msg) -> Html msg
navDropdownLink attributes children =
    a
        ([ class "w3-bar-item w3-button"
         , href "#"
         ]
            ++ attributes
        )
        children



-- Sidebar --


sideBar : List (Attribute msg) -> List (Html msg) -> Html msg
sideBar attributes children =
    nav
        (class "w3-sidebar w3-bar-block w3-collapse w3-large w3-theme-l5 w3-animate-left"
            :: attributes
        )
        children


closeSideBarButton : List (Attribute msg) -> Html msg
closeSideBarButton attributes =
    a
        ([ class "w3-right w3-xlarge w3-padding-large w3-hover-black w3-hide-large"
         , href "#"
         , title "Close Menu"
         ]
            ++ attributes
        )
        [ i [ class "fa fa-remove" ] [] ]


sideBarTitle : List (Attribute msg) -> List (Html msg) -> Html msg
sideBarTitle attributes children =
    h4 (class "w3-bar-item" :: attributes)
        [ b [] children ]


sideBarLink : List (Attribute msg) -> List (Html msg) -> Html msg
sideBarLink attributes children =
    a
        ([ class "w3-bar-item w3-button w3-hover-black w3-medium"
         , href "#"
         ]
            ++ attributes
        )
        children


smallSideBarLink : List (Attribute msg) -> List (Html msg) -> Html msg
smallSideBarLink attributes children =
    a
        ([ class "w3-bar-item w3-button w3-hover-black w3-small"
         , href "#"
         ]
            ++ attributes
        )
        children


sideBarShadow : List (Attribute msg) -> Html msg
sideBarShadow attributes =
    div
        ([ class "w3-overlay w3-hide-large"
         , style "cursor" "pointer"
         , title "Close side menu"
         ]
            ++ attributes
        )
        []



-- Lists & Tables --


list : List (Attribute msg) -> List (Html msg) -> Html msg
list attributes children =
    ul (class "w3-ul" :: attributes)
        (List.map (\c -> li [] [ c ]) <| List.filter (\c -> c /= text "") children)



-- Main Structuring --


mainContainer : List (Attribute msg) -> List (Html msg) -> Html msg
mainContainer attributes children =
    div
        ([ class "w3-main"
         , style "margin-left" "250px"
         ]
            ++ attributes
        )
        children


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attributes children =
    div (class "w3-row w3-padding-32" :: attributes)
        children
