module Styles exposing (..)


import Html             exposing (..)
import Html.Attributes  exposing ( style )

type alias RGB = (Int, Int, Int)


treePageStyle : Attribute msg
treePageStyle = style [
      ("background-color", "#aaa")
    , ("display", "flex")
    , ("align-items", "start")
    , ("justify-content", "center")
    , ("flex-grow", "1")
    , ("font-size", "12px")
    , ("font-weight", "bold")
    , ("padding-bottom", "20px")
    ]

treePageContentStyle : Attribute msg
treePageContentStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("justify-content", "flex-start")
    , ("align-items", "center")
    , ("flex-grow", "1")
    ]


personWithOthersStyle : Attribute msg
personWithOthersStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("align-items", "center")
    , ("margin-right", "10px")
    ]



personBareStyle : Attribute msg
personBareStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("align-items", "center")
    ]

-- depth could be negative as well
-- bigger number means older
-- for descendants (when starting from olderst) it should go
-- -1, -2, -3 etc
-- for ancestors (when starting from youngest) it should go
-- 0, 1, 2
personBoxStyle : Int -> Attribute msg
personBoxStyle depth = style [
      ("display", "flex")
    , ("padding", "5px")
    , ("margin", "10px 5px")
    , ("border", "1px solid #333")
    , ("border-radius", "15px")
    , ("background-color", getColor depth)
    , ("z-index", "2")
    , ("box-shadow", "2px 2px 5px 1px #333")
    , ("cursor", "pointer")
    ]

spouseStyle : Attribute msg
spouseStyle = style [
      ("display", "flex")
    , ("padding", "5px")
    , ("margin", "10px 0 -15px 0")
    , ("border-radius", "15px")
    , ("background-color", "#555")
    , ("color", "#ddd")
    , ("z-index", "1")
    , ("box-shadow", "2px 2px 5px 1px #333")
    , ("cursor", "pointer")
    ]

branchesStyle : Attribute msg
branchesStyle = style [
      ("display", "flex")
    , ("flex-direction", "row")
    , ("justify-content", "center")
    , ("border-radius", "15px")
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



gradientSteps = 7

greenBrownGradients : List RGB
greenBrownGradients = calculateGradients (204,228,175) (200,175,120) gradientSteps


getColor : Int -> String
getColor i = (\(r,g,b) ->
    "rgb(" ++ (toString r) ++ "," ++ (toString g) ++ "," ++ (toString b) ++ ")")
    <| Maybe.withDefault (0,0,0)
    <| List.head
    <| List.drop (if i < 0 then gradientSteps + i else i) greenBrownGradients
