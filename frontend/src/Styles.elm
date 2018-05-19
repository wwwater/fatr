module Styles exposing (..)


import Html             exposing (..)
import Html.Attributes  exposing ( style )

type alias RGB = (Int, Int, Int)

formStyle : Attribute msg
formStyle = style [
      ("padding", "15px 20px")
    , ("border-radius", "15px")
    , ("border", "none")
    , ("font-size", "14px")
    ]

pageStyle : Attribute msg
pageStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("font-size", "12px")
    , ("font-weight", "bold")
    , ("padding", "20px 0")
    , ("margin-left", "auto")
    , ("margin-right", "auto")
    , ("flex-grow", "1")
    , ("padding-top", "80px") -- menu
    ]

treeStyle : Attribute msg
treeStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("flex-grow", "1")
    , ("align-items", "center")
    ]


personWithOthersStyle : Attribute msg
personWithOthersStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("align-items", "center")
    , ("justify-content", "flex-end")
    ]



personBareStyle : Attribute msg
personBareStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("align-items", "center")
    , ("position", "relative")
    ]

personBoxStyle : Maybe String -> Attribute msg
personBoxStyle maybeBirthday = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("padding", "5px")
    , ("margin", "10px 5px")
    , ("border", "1px solid #333")
    , ("border-radius", "15px")
    , ("background-color", getColor maybeBirthday)
    , ("position", "relative")
    , ("box-shadow", "2px 2px 5px 1px #333")
    , ("cursor", "pointer")
    ]

spouseStyle : Attribute msg
spouseStyle = style [
      ("display", "flex")
    , ("padding", "5px")
    , ("margin", "10px 0 -15px 0")
    , ("border-radius", "15px")
    , ("background-color", "#666")
    , ("color", "#ddd")
    , ("position", "relative")
    , ("box-shadow", "2px 2px 5px 1px #333")
    , ("cursor", "pointer")
    ]

branchesStyle : Attribute msg
branchesStyle = style [
      ("display", "flex")
    , ("flex-direction", "row")
    , ("border-radius", "30px")
    , ("margin", "5px")
    ]






calculateGradients : RGB -> RGB -> Int -> List RGB
calculateGradients (r1,g1,b1) (r2,g2,b2) steps =
    let dx x1 x2 s = ((toFloat x2) - (toFloat x1)) / (toFloat s)
        increment x dx n = round ((toFloat x) + dx * (toFloat n))
        dr = dx r1 r2 steps
        dg = dx g1 g2 steps
        db = dx b1 b2 steps
        ss = List.range 0 (steps - 1)
        rgbs = List.repeat steps (r1,g1,b1) in
        List.map2 (\i (r,g,b) ->
            (increment r dr i, increment g dg i, increment b db i)) ss rgbs



gradientSteps = 150

brown : RGB
brown = (173,147,131)

green : RGB
green = (198,235,159)

brownToGreenGradients : List RGB
brownToGreenGradients = calculateGradients brown green gradientSteps

mostAncientBirthYear = 1870

getColor : Maybe String -> String
getColor maybeBirthday =
  let year = Result.withDefault mostAncientBirthYear
    <| String.toInt
    <| Maybe.withDefault ""
    <| Maybe.map (\b -> String.left 4 b) maybeBirthday
  in
  (\(r,g,b) ->
    "rgb(" ++ (toString r) ++ "," ++ (toString g) ++ "," ++ (toString b) ++ ")")
    <| Maybe.withDefault (200,200,200)
    <| List.head
    <| List.drop (max 0 (year - mostAncientBirthYear)) brownToGreenGradients
