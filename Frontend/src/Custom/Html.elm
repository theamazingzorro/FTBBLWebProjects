module Custom.Html exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)



-- Attributes --


rightAlign : Attribute msg
rightAlign =
    class "w3-right"



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
        ([ class "w3-bar-item w3-button w3-padding-large w3-hide-large w3-right"
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



-- Headers & Text --


mainHeader : List (Attribute msg) -> List (Html msg) -> Html msg
mainHeader attributes children =
    h1 (class "w3-text-teal" :: attributes)
        children


subHeader : List (Attribute msg) -> List (Html msg) -> Html msg
subHeader attributes children =
    h2 (class "w3-text-teal" :: attributes)
        children


emphasisText : List (Attribute msg) -> List (Html msg) -> Html msg
emphasisText attributes children =
    h3 attributes children


errorText : List (Attribute msg) -> List (Html msg) -> Html msg
errorText attributes children =
    div (class "w3-text-red" :: attributes)
        children



-- Lists & Tables --


list : List (Attribute msg) -> List (Html msg) -> Html msg
list attributes children =
    ul (class "w3-ul" :: attributes)
        (List.map (\c -> li [] [ c ]) <| List.filter (\c -> c /= text "") children)


table : List (Attribute msg) -> List (Html msg) -> Html msg
table attributes children =
    Html.table (class "w3-table w3-striped w3-bordered w3-hoverable" :: attributes)
        children


tableHead : List (Attribute msg) -> List ( List (Attribute msg), List (Html msg) ) -> Html msg
tableHead attributes children =
    thead attributes
        [ tr [ class "w3-theme" ]
            (List.map (\( a, h ) -> th a h) children)
        ]


tableBody : List (Attribute msg) -> List (Html msg) -> Html msg
tableBody attributes children =
    tbody attributes children


tableRow : List (Attribute msg) -> List ( List (Attribute msg), List (Html msg) ) -> Html msg
tableRow attributes children =
    tr attributes
        (List.map (\( a, h ) -> td a h) children)



-- Buttons --


addButton : List (Attribute msg) -> List (Html msg) -> Html msg
addButton attributes children =
    a
        ([ class "w3-button w3-green"
         , href "#"
         ]
            ++ attributes
        )
        children


optionButton : List (Attribute msg) -> List (Html msg) -> Html msg
optionButton attributes children =
    a
        ([ class "w3-button w3-theme"
         , href "#"
         ]
            ++ attributes
        )
        children


warnButton : List (Attribute msg) -> List (Html msg) -> Html msg
warnButton attributes children =
    a
        ([ class "w3-button w3-red"
         , href "#"
         ]
            ++ attributes
        )
        children


roundButton : List (Attribute msg) -> List (Html msg) -> Html msg
roundButton attributes children =
    a
        ([ class "w3-button w3-circle w3-large w3-card-4"
         , href "#"
         ]
            ++ attributes
        )
        children


pageLink : List (Attribute msg) -> List (Html msg) -> Html msg
pageLink attributes children =
    a (href "#" :: attributes) children



-- Main Structuring --


mainContainer : List (Attribute msg) -> List (Html msg) -> Html msg
mainContainer attributes children =
    div
        ([ class "w3-main flex-wrapper"
         , style "margin-left" "250px"
         ]
            ++ attributes
        )
        children


pageContent : List (Attribute msg) -> List (Html msg) -> Html msg
pageContent attributes children =
    div []
        [ div (class "main-page-content" :: attributes) children ]


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attributes children =
    div (class "w3-row w3-padding-32" :: attributes)
        children


colHalf : List (Attribute msg) -> List (Html msg) -> Html msg
colHalf attributes children =
    div (class "w3-half w3-container" :: attributes)
        children


colThird : List (Attribute msg) -> List (Html msg) -> Html msg
colThird attributes children =
    div (class "w3-third w3-container" :: attributes)
        children


colTwoThird : List (Attribute msg) -> List (Html msg) -> Html msg
colTwoThird attributes children =
    div (class "w3-twothird w3-container" :: attributes)
        children


colQuarter : List (Attribute msg) -> List (Html msg) -> Html msg
colQuarter attributes children =
    div (class "w3-quarter w3-container" :: attributes)
        children


colThreeQuarter : List (Attribute msg) -> List (Html msg) -> Html msg
colThreeQuarter attributes children =
    div (class "w3-threequarter w3-container" :: attributes)
        children


footer : List (Attribute msg) -> List (Html msg) -> List (Html msg) -> Html msg
footer attributes mainChildren subChildren =
    Html.footer (id "footer" :: attributes)
        [ div [ class "w3-container w3-theme-l2 w3-padding-32" ]
            mainChildren
        , div [ class "w3-container w3-theme-l1" ]
            subChildren
        ]



-- Pagination --


pageBar : List (Attribute msg) -> List (Html msg) -> Html msg
pageBar attributes children =
    div (class "w3-center w3-padding-32" :: attributes)
        [ div [ class "w3-bar" ]
            children
        ]


pageBarButton : List (Attribute msg) -> List (Html msg) -> Html msg
pageBarButton attributes children =
    div
        (class "w3-bar-item w3-button w3-hover-teal" :: attributes)
        children


pageBarFiller : List (Attribute msg) -> List (Html msg) -> Html msg
pageBarFiller attributes children =
    div
        (class "w3-bar-item" :: attributes)
        children


selectedPageBarButton : List (Attribute msg) -> List (Html msg) -> Html msg
selectedPageBarButton attributes children =
    div
        (class "w3-bar-item w3-button w3-teal w3-hover-grey" :: attributes)
        children
