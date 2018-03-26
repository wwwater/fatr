module Global exposing ( handleServerError
                       , onlyUpdateModel
                       )

import Http


getError : Http.Error -> (String, Maybe Int)
getError err =
    case err of
        Http.BadStatus badStatus ->
            (case badStatus.status.code of
                404 -> "Человека с таким номером нет в базе данных!"
                401 -> badStatus.body
                _ -> badStatus.status.message
            , Just badStatus.status.code)
        Http.BadUrl text ->
            ("Неправильный запрос: " ++ text, Nothing)
        Http.Timeout ->
            ("Таймаут. Попробуйте перезагрузить страницу.", Nothing)
        Http.BadPayload message _ ->
            ("Неправильный запрос: " ++ message, Nothing)
        Http.NetworkError ->
            ("У Вас отключили интернет (шутка)", Nothing)

handleServerError : { m | error: Maybe String } -> Http.Error ->
    ( { m | error : Maybe String }, Cmd b )
handleServerError model err =
    let (errorAsString, code) = getError err
        _ = Debug.log "An error occured in request" errorAsString in
    ( { model | error = Just errorAsString }
    , Cmd.none
    )

onlyUpdateModel : m -> ( m, Cmd msg )
onlyUpdateModel model = ( model, Cmd.none )

