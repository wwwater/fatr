module ConnectionsUtil      exposing ( calculateConnectingLines )

import ServerApi            exposing (..)

calculateConnectingLines : Person -> List (Int, Int)
calculateConnectingLines person =
  []
  |> addDescendantsConnections person
  |> addAncestorsConnections (Just person)

addDescendantsConnections : Person -> List (Int, Int) -> List (Int, Int)
addDescendantsConnections person connections =
  List.foldl(\childrenWithSpouse connections ->
    let maybeSpouse = getSpouse childrenWithSpouse
        children = getChildren childrenWithSpouse
        -- if spouse is present, draw connection to children from the spouse
        -- otherwise from the person itself
        spouseOrElsePerson = Maybe.withDefault person maybeSpouse in
        connections
        |> addConnection person maybeSpouse
        |> addChildrenConnections spouseOrElsePerson children)
    connections
    person.children

addChildrenConnections : Person -> List Person -> List (Int, Int) -> List (Int, Int)
addChildrenConnections person children connections =
  List.foldl (\child connections ->
      connections
      |> addConnection person (Just child)
      |> addDescendantsConnections child)
    connections children


addAncestorsConnections : Maybe Person -> List (Int, Int) -> List (Int, Int)
addAncestorsConnections maybePerson connections =
  case maybePerson of
    Just person ->
      let maybeFather = getFather person
          maybeMother = getMother person
        in (connections
            |> addConnection person maybeFather
            |> addConnection person maybeMother
            |> addAncestorsConnections maybeFather
            |> addAncestorsConnections maybeMother)
    Nothing -> connections


addConnection : Person -> Maybe Person -> List (Int, Int) -> List (Int, Int)
addConnection person maybeAnotherPerson connections =
  case maybeAnotherPerson of
    Just anotherPerson ->
      (person.id, anotherPerson.id) :: connections
    Nothing -> connections
