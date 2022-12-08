module Custom.Attributes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- General --


textCentered : Attribute msg
textCentered =
    style "text-align" "center"


centered : Attribute msg
centered =
    style "margin" "1em auto"


visuallyHidden : Attribute msg
visuallyHidden =
    class "visually-hidden"


maxWidth : String -> Attribute msg
maxWidth =
    style "max-width"


backgroundColor : String -> Attribute msg
backgroundColor =
    style "background-color"


padding : String -> Attribute msg
padding =
    style "padding"



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


tableButtonColumn : Int -> List (Attribute msg)
tableButtonColumn btnCount =
    [ width <| btnCount * 100
    , style "text-align" "right"
    ]



-- Buttons --


button : Attribute msg
button =
    type_ "button"


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


textButton : msg -> List (Attribute msg)
textButton event =
    [ onClick <| event
    , style "box-shadow" "2px 2px 2px grey"
    , style "padding" "2px"
    ]



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


formCheckbox : String -> List (Attribute msg) -> List (Attribute msg)
formCheckbox thisId otherAttributes =
    [ class "form-check-input"
    , style "margin" "auto 15px"
    , type_ "checkbox"
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



-- Carousel --


carouselContainer : List (Attribute msg)
carouselContainer =
    [ class "carousel slide carousel-dark"
    , attribute "data-bs-interval" "false"
    , attribute "data-bs-wrap" "false"
    , backgroundColor "#eee"
    ]


carouselInner : Attribute msg
carouselInner =
    class "carousel-inner"


carouselIndicators : Attribute msg
carouselIndicators =
    class "carousel-indicators"


dataBsTarget : String -> Attribute msg
dataBsTarget =
    attribute "data-bs-target"


carouselNextButton : Attribute msg
carouselNextButton =
    class "carousel-control-next"


carouselNextIcon : Attribute msg
carouselNextIcon =
    class "carousel-control-next-icon"


carouselPrevButton : Attribute msg
carouselPrevButton =
    class "carousel-control-prev"


carouselPrevIcon : Attribute msg
carouselPrevIcon =
    class "carousel-control-prev-icon"


carouselItem : List (Attribute msg)
carouselItem =
    [ class "carousel-item"
    , style "padding" "3em 5em 5em"
    ]


carouselItemEntry : List (Attribute msg)
carouselItemEntry =
    [ textCentered
    , backgroundColor "#aaa"
    , maxWidth "70%"
    , centered
    , padding "0.5em"
    ]
