module ServerApi exposing (..)

import Json.Decode as JsonD
import Json.Encode as JsonE
import Json.Decode.Pipeline as JsonPipeline
import Http


type alias Credentials =
    { username : String
    , password : String
    }

type alias Jwt = String

type alias Person =
    { id : Int
    , givenName : Maybe String
    , surname : Maybe String
    , patronymic : Maybe String
    , birthday : Maybe String
    , deathday : Maybe String
    , parents : Parents
    , children : List Children
    , photo : Maybe String
    , about : Maybe String
    }

type Parents = Parents
    { mother : Maybe Person
    , father : Maybe Person
    }

type Children = Children
    { spouse : Maybe Person
    , childrenWithSpouse : List Person
    }

comparatorByAge : Person -> Person -> Order
comparatorByAge p1 p2 =
  compare (Maybe.withDefault "" p1.birthday) (Maybe.withDefault "" p2.birthday)


getMother : Person -> Maybe Person
getMother person =
    let (Parents parents) = person.parents in parents.mother

getFather : Person -> Maybe Person
getFather person =
    let (Parents parents) = person.parents in parents.father

getSpouse : Children -> Maybe Person
getSpouse (Children children) =
    children.spouse

getChildren : Children -> List Person
getChildren (Children children) =
  List.sortWith comparatorByAge <| children.childrenWithSpouse



baseUrl : String
baseUrl =
    "http://localhost:8081"


getJwt : Credentials -> (Result Http.Error Jwt -> msg) -> Cmd msg
getJwt credentials msg =
    Http.post (baseUrl ++ "/jwt")
        (Http.stringBody "application/json" <| encodeCredentials credentials)
        jwtDecoder
        |> Http.send msg


searchPersons : String -> Jwt -> (Result Http.Error (List Person) -> msg) -> Cmd msg
searchPersons searchString jwt msg =
    Http.request
        { method = "GET"
        , url = baseUrl ++ "/person/search/" ++ searchString
        , headers = [ Http.header "jwt" jwt ]
        , body = Http.emptyBody
        , expect = Http.expectJson (JsonD.list personDecoder)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg

getPersonWithAbout : Int -> Jwt -> (Result Http.Error Person -> msg) -> Cmd msg
getPersonWithAbout personId jwt msg =
    Http.request
        { method = "GET"
        , url = baseUrl ++ "/person/" ++ toString personId
        , headers = [ Http.header "jwt" jwt ]
        , body = Http.emptyBody
        , expect = Http.expectJson personDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg

getPersonTree : Int -> Jwt -> (Result Http.Error Person -> msg) -> Cmd msg
getPersonTree personId jwt msg =
    Http.request
        { method = "GET"
        , url = baseUrl ++ "/person/" ++ toString personId ++ "/tree"
        , headers = [ Http.header "jwt" jwt ]
        , body = Http.emptyBody
        , expect = Http.expectJson personDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg

getPersonSiblings : Int -> Jwt -> (Result Http.Error (List (List Person)) -> msg) -> Cmd msg
getPersonSiblings personId jwt msg =
    Http.request
        { method = "GET"
        , url = baseUrl ++ "/person/" ++ toString personId ++ "/siblings"
        , headers = [ Http.header "jwt" jwt ]
        , body = Http.emptyBody
        , expect = Http.expectJson (JsonD.list (JsonD.list personDecoder))
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send msg


encodeCredentials : Credentials -> String
encodeCredentials credentials =
    JsonE.encode 0 <|
        JsonE.object
            [ ("username", JsonE.string credentials.username)
            , ("password", JsonE.string credentials.password)
            ]

jwtDecoder : JsonD.Decoder Jwt
jwtDecoder = JsonD.field "token" JsonD.string

personDecoder : JsonD.Decoder Person
personDecoder =
  JsonPipeline.decode Person
    |> JsonPipeline.required "id" JsonD.int
    |> JsonPipeline.required "givenName" (JsonD.nullable JsonD.string)
    |> JsonPipeline.required "surname" (JsonD.nullable JsonD.string)
    |> JsonPipeline.required "patronymic" (JsonD.nullable JsonD.string)
    |> JsonPipeline.required "birthday" (JsonD.nullable JsonD.string)
    |> JsonPipeline.required "deathday" (JsonD.nullable JsonD.string)
    |> JsonPipeline.required "parents" (JsonD.lazy (\_ -> parentsDecoder))
    |> JsonPipeline.required "children" (JsonD.lazy (\_ -> JsonD.list childrenDecoder))
    |> JsonPipeline.required "photo" (JsonD.nullable JsonD.string)
    |> JsonPipeline.required "about" (JsonD.nullable JsonD.string)

parentsDecoder : JsonD.Decoder Parents
parentsDecoder =
    JsonD.map2 (\m f -> Parents {mother = m, father = f})
        (JsonD.field "mother" (JsonD.maybe personDecoder))
        (JsonD.field "father" (JsonD.maybe personDecoder))

childrenDecoder : JsonD.Decoder Children
childrenDecoder =
    JsonD.map2 (\s c -> Children {spouse = s, childrenWithSpouse = c})
        (JsonD.field "spouse" (JsonD.lazy (\_ -> JsonD.maybe personDecoder)))
        (JsonD.field "childrenWithSpouse" (JsonD.lazy (\_ -> JsonD.list personDecoder)))

