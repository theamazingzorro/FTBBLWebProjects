module Custom.Attributes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- Grid --


mainContainer : Attribute msg
mainContainer =
    class "container"


row : List (Attribute msg)
row =
    [ class "row", style "padding-bottom" "6px" ]


col : Attribute msg
col =
    class "col"



-- Nav --


mainNavBar : Attribute msg
mainNavBar =
    class "navbar navbar-expand-lg navbar-light bg-light"


navBarBrand : Attribute msg
navBarBrand =
    class "navbar-brand"


navBarCollapsable : Attribute msg
navBarCollapsable =
    class "collapse navbar-collapse"


navBarLinkList : Attribute msg
navBarLinkList =
    class "navbar-nav"


navItem : Attribute msg
navItem =
    class "nav-item"


navLink : Attribute msg
navLink =
    class "nav-link"


navDropDownContainer : Attribute msg
navDropDownContainer =
    class "nav-item dropdown"


navDropDownTitleLink : List (Attribute msg)
navDropDownTitleLink =
    [ class "nav-link dropdown-toggle"
    , attribute "data-bs-toggle" "dropdown"
    ]


navDropDownMenu : Attribute msg
navDropDownMenu =
    class "dropdown-menu"


navDropDownItem : Attribute msg
navDropDownItem =
    class "dropdown-item"



-- Tables --


table : Attribute msg
table =
    class "table table-striped table-hover"


tableButtonColumn : Int -> Attribute msg
tableButtonColumn btnCount =
    width <| btnCount * 100



-- Buttons --


rightSideButtons : Attribute msg
rightSideButtons =
    class "btn-group float-end"


refreshButton : Attribute msg
refreshButton =
    class "btn btn-sm btn-secondary float-end"


addButton : Attribute msg
addButton =
    class "btn btn-success"


deleteButton : List (Attribute msg)
deleteButton =
    [ class "btn btn-danger"
    , style "margin-left" "6px"
    ]


editButton : List (Attribute msg)
editButton =
    [ class "btn btn-secondary"
    , style "margin-left" "6px"
    ]


submitButton : Attribute msg
submitButton =
    class "btn btn-primary"



-- Errors --


errorMessage : Attribute msg
errorMessage =
    style "color" "#d00"



-- Forms --


formEntry : Attribute msg
formEntry =
    class "mb-3"


formLabel : String -> List (Attribute msg)
formLabel id =
    [ class "form-label"
    , for id
    ]


formInput : String -> List (Attribute msg) -> List (Attribute msg)
formInput thisId otherAttributes =
    [ class "form-control"
    , id thisId
    ]
        ++ otherAttributes


formPasswordInput : String -> List (Attribute msg) -> List (Attribute msg)
formPasswordInput thisId otherAttributes =
    [ class "form-control"
    , type_ "password"
    , id thisId
    ]
        ++ otherAttributes


formDropdown : String -> List (Attribute msg) -> List (Attribute msg)
formDropdown thisId otherAttributes =
    formInput thisId <|
        class "form-select"
            :: otherAttributes


formInputLink : String -> msg -> List (Attribute msg) -> List (Attribute msg)
formInputLink thisId clickMsg otherAttributes =
    formInput thisId
        [ onClick clickMsg
        , class "btn-link"
        ]
        ++ otherAttributes
