module Leaderboard exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Http
import Round
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- TYPES


type alias ValidModel =
    { id : String
    , name : String
    , paramsM : Maybe Float
    , modalities : String
    , org : String
    , link : String
    }


type alias ValidTask =
    { id : String
    , name : String
    , metric : String
    , description : String
    , modality : String
    , nImages : String
    , meanOutputTokens : String
    , questionStyle : String
    , domain : String
    , capability : String
    , minScore : Maybe Float
    , randomScore : Maybe Float
    , maxScore : Maybe Float
    , url : String
    }


type alias ValidScore =
    { modelId : String
    , taskId : String
    , score : Float
    , reportedBy : String
    , source : String
    , notes : String
    }


type alias Leaderboard =
    { models : List ValidModel
    , tasks : List ValidTask
    , scoresByModel : Dict String (Dict String ValidScore)
    }


type Warning
    = ScoreReferencesUnknownModel { line : Int, modelId : String }
    | ScoreReferencesUnknownTask { line : Int, taskId : String }
    | ModelHasNoScores { modelId : String, modelName : String }
    | TaskHasNoScores { taskId : String, taskName : String }
    | CsvParseError { file : String, line : Int, problem : String }
    | ScoreNotNumeric { line : Int, modelId : String, taskId : String, raw : String }


type Hint
    = ModelMissingField { modelId : String, modelName : String, field : String }
    | TaskMissingField { taskId : String, taskName : String, field : String }
    | ScoreMissingField { modelId : String, taskId : String, field : String }


type LoadState
    = Loading { models : Maybe String, tasks : Maybe String, scores : Maybe String }
    | Ready Leaderboard (List Warning) (List Hint)
    | FetchError String


type Order
    = Asc
    | Desc


type alias Model =
    { loading : LoadState
    , sortKey : String
    , sortOrder : Order
    , warningsExpanded : Bool
    , hintsExpanded : Bool
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { loading = Loading { models = Nothing, tasks = Nothing, scores = Nothing }
      , sortKey = "model"
      , sortOrder = Asc
      , warningsExpanded = False
      , hintsExpanded = False
      }
    , Cmd.batch
        [ Http.get { url = "data/models.csv", expect = Http.expectString GotModels }
        , Http.get { url = "data/tasks.csv", expect = Http.expectString GotTasks }
        , Http.get { url = "data/scores.csv", expect = Http.expectString GotScores }
        ]
    )



-- UPDATE


type Msg
    = GotModels (Result Http.Error String)
    | GotTasks (Result Http.Error String)
    | GotScores (Result Http.Error String)
    | Sort String
    | ToggleWarnings
    | ToggleHints


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotModels result ->
            case model.loading of
                Loading state ->
                    case result of
                        Ok raw ->
                            tryFinalize { state | models = Just raw } model

                        Err e ->
                            ( { model | loading = FetchError ("Failed to fetch models.csv: " ++ httpErrorToString e) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTasks result ->
            case model.loading of
                Loading state ->
                    case result of
                        Ok raw ->
                            tryFinalize { state | tasks = Just raw } model

                        Err e ->
                            ( { model | loading = FetchError ("Failed to fetch tasks.csv: " ++ httpErrorToString e) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotScores result ->
            case model.loading of
                Loading state ->
                    case result of
                        Ok raw ->
                            tryFinalize { state | scores = Just raw } model

                        Err e ->
                            ( { model | loading = FetchError ("Failed to fetch scores.csv: " ++ httpErrorToString e) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Sort key ->
            if key == model.sortKey then
                ( { model | sortOrder = flipOrder model.sortOrder }, Cmd.none )

            else
                ( { model | sortKey = key, sortOrder = Desc }, Cmd.none )

        ToggleWarnings ->
            ( { model | warningsExpanded = not model.warningsExpanded }, Cmd.none )

        ToggleHints ->
            ( { model | hintsExpanded = not model.hintsExpanded }, Cmd.none )


tryFinalize : { models : Maybe String, tasks : Maybe String, scores : Maybe String } -> Model -> ( Model, Cmd Msg )
tryFinalize state model =
    case ( state.models, state.tasks, state.scores ) of
        ( Just m, Just t, Just s ) ->
            let
                ( leaderboard, warnings, hints ) =
                    parseAndValidate m t s
            in
            ( { model | loading = Ready leaderboard warnings hints }, Cmd.none )

        _ ->
            ( { model | loading = Loading state }, Cmd.none )


flipOrder : Order -> Order
flipOrder order =
    case order of
        Asc ->
            Desc

        Desc ->
            Asc


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "bad URL: " ++ url

        Http.Timeout ->
            "request timed out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "HTTP " ++ String.fromInt status

        Http.BadBody body ->
            "bad response: " ++ body



-- CSV PARSING


parseAndValidate : String -> String -> String -> ( Leaderboard, List Warning, List Hint )
parseAndValidate rawModels rawTasks rawScores =
    let
        ( models, modelWarnings ) =
            parseModels rawModels

        ( tasks, taskWarnings ) =
            parseTasks rawTasks

        ( scores, scoreWarnings ) =
            parseScores rawScores

        modelIds =
            Set.fromList (List.map .id models)

        taskIds =
            Set.fromList (List.map .id tasks)

        -- Check for dangling references
        danglingModelWarnings =
            scores
                |> List.filterMap
                    (\s ->
                        if Set.member s.modelId modelIds then
                            Nothing

                        else
                            Just (ScoreReferencesUnknownModel { line = 0, modelId = s.modelId })
                    )
                |> dedup warningKey

        danglingTaskWarnings =
            scores
                |> List.filterMap
                    (\s ->
                        if Set.member s.taskId taskIds then
                            Nothing

                        else
                            Just (ScoreReferencesUnknownTask { line = 0, taskId = s.taskId })
                    )
                |> dedup warningKey

        -- Keep only scores with valid references
        validScores =
            List.filter (\s -> Set.member s.modelId modelIds && Set.member s.taskId taskIds) scores

        -- Check for models/tasks with no scores
        scoredModelIds =
            Set.fromList (List.map .modelId validScores)

        scoredTaskIds =
            Set.fromList (List.map .taskId validScores)

        noScoreModelWarnings =
            models
                |> List.filterMap
                    (\m ->
                        if Set.member m.id scoredModelIds then
                            Nothing

                        else
                            Just (ModelHasNoScores { modelId = m.id, modelName = m.name })
                    )

        noScoreTaskWarnings =
            tasks
                |> List.filterMap
                    (\t ->
                        if Set.member t.id scoredTaskIds then
                            Nothing

                        else
                            Just (TaskHasNoScores { taskId = t.id, taskName = t.name })
                    )

        -- Build nested dict: model_id -> (task_id -> score)
        scoresByModel =
            List.foldl
                (\s acc ->
                    let
                        inner =
                            Dict.get s.modelId acc |> Maybe.withDefault Dict.empty
                    in
                    Dict.insert s.modelId (Dict.insert s.taskId s inner) acc
                )
                Dict.empty
                validScores

        allWarnings =
            modelWarnings
                ++ taskWarnings
                ++ scoreWarnings
                ++ danglingModelWarnings
                ++ danglingTaskWarnings
                ++ noScoreModelWarnings
                ++ noScoreTaskWarnings

        -- Hints: missing/empty optional fields
        modelHints =
            models |> List.concatMap checkModelFields

        taskHints =
            tasks |> List.concatMap checkTaskFields

        scoreHints =
            validScores |> List.concatMap checkScoreFields

        allHints =
            modelHints ++ taskHints ++ scoreHints
    in
    ( { models = models, tasks = tasks, scoresByModel = scoresByModel }
    , allWarnings
    , allHints
    )


checkModelFields : ValidModel -> List Hint
checkModelFields m =
    let
        check field value =
            if String.isEmpty value then
                [ ModelMissingField { modelId = m.id, modelName = m.name, field = field } ]

            else
                []
    in
    (case m.paramsM of
        Nothing ->
            [ ModelMissingField { modelId = m.id, modelName = m.name, field = "params_m" } ]

        Just _ ->
            []
    )
        ++ check "modalities" m.modalities
        ++ check "org" m.org
        ++ check "link" m.link


checkTaskFields : ValidTask -> List Hint
checkTaskFields t =
    let
        check field value =
            if String.isEmpty value then
                [ TaskMissingField { taskId = t.id, taskName = t.name, field = field } ]

            else
                []

        checkMaybe field value =
            case value of
                Nothing ->
                    [ TaskMissingField { taskId = t.id, taskName = t.name, field = field } ]

                Just _ ->
                    []
    in
    check "metric" t.metric
        ++ check "description" t.description
        ++ check "modality" t.modality
        ++ check "n_images" t.nImages
        ++ check "mean_output_tokens" t.meanOutputTokens
        ++ check "question_style" t.questionStyle
        ++ check "domain" t.domain
        ++ check "capability" t.capability
        ++ checkMaybe "min_score" t.minScore
        ++ checkMaybe "random_score" t.randomScore
        ++ checkMaybe "max_score" t.maxScore
        ++ check "url" t.url


checkScoreFields : ValidScore -> List Hint
checkScoreFields s =
    let
        check field value =
            if String.isEmpty value then
                [ ScoreMissingField { modelId = s.modelId, taskId = s.taskId, field = field } ]

            else
                []
    in
    check "reported_by" s.reportedBy
        ++ check "source" s.source


{-| Deduplicate a list based on a key function, preserving first occurrence.
-}
dedup : (a -> String) -> List a -> List a
dedup toKey items =
    let
        step item ( seen, acc ) =
            let
                k =
                    toKey item
            in
            if Set.member k seen then
                ( seen, acc )

            else
                ( Set.insert k seen, item :: acc )
    in
    List.foldl step ( Set.empty, [] ) items |> Tuple.second |> List.reverse


warningKey : Warning -> String
warningKey w =
    case w of
        ScoreReferencesUnknownModel r ->
            "unknown-model:" ++ r.modelId

        ScoreReferencesUnknownTask r ->
            "unknown-task:" ++ r.taskId

        ModelHasNoScores r ->
            "no-scores-model:" ++ r.modelId

        TaskHasNoScores r ->
            "no-scores-task:" ++ r.taskId

        CsvParseError r ->
            "parse:" ++ r.file ++ ":" ++ String.fromInt r.line

        ScoreNotNumeric r ->
            "not-numeric:" ++ String.fromInt r.line ++ ":" ++ r.modelId ++ ":" ++ r.taskId


parseModels : String -> ( List ValidModel, List Warning )
parseModels raw =
    let
        lines =
            String.lines raw |> List.filter (\l -> String.trim l /= "")
    in
    case lines of
        [] ->
            ( [], [ CsvParseError { file = "models.csv", line = 0, problem = "file is empty" } ] )

        _ :: rest ->
            let
                results =
                    List.indexedMap (\i line -> parseModelRow (i + 2) line) rest
            in
            ( List.filterMap Result.toMaybe results
            , List.filterMap
                (\r ->
                    case r of
                        Err w ->
                            Just w

                        Ok _ ->
                            Nothing
                )
                results
            )


parseModelRow : Int -> String -> Result Warning ValidModel
parseModelRow lineNum line =
    let
        cells =
            splitCsvLine line
    in
    case cells of
        [ id, name, paramsM, modalities, org, link ] ->
            Ok
                { id = id
                , name = name
                , paramsM = String.toFloat paramsM
                , modalities = modalities
                , org = org
                , link = link
                }

        _ ->
            Err
                (CsvParseError
                    { file = "models.csv"
                    , line = lineNum
                    , problem = "expected 6 columns, got " ++ String.fromInt (List.length cells)
                    }
                )


parseTasks : String -> ( List ValidTask, List Warning )
parseTasks raw =
    let
        lines =
            String.lines raw |> List.filter (\l -> String.trim l /= "")
    in
    case lines of
        [] ->
            ( [], [ CsvParseError { file = "tasks.csv", line = 0, problem = "file is empty" } ] )

        _ :: rest ->
            let
                results =
                    List.indexedMap (\i line -> parseTaskRow (i + 2) line) rest
            in
            ( List.filterMap Result.toMaybe results
            , List.filterMap
                (\r ->
                    case r of
                        Err w ->
                            Just w

                        Ok _ ->
                            Nothing
                )
                results
            )


parseTaskRow : Int -> String -> Result Warning ValidTask
parseTaskRow lineNum line =
    let
        cells =
            splitCsvLine line
    in
    case cells of
        [ id, name, metric, description, modality, nImages, meanOutputTokens, questionStyle, domain, capability, minScore, randomScore, maxScore, url ] ->
            Ok
                { id = id
                , name = name
                , metric = metric
                , description = description
                , modality = modality
                , nImages = nImages
                , meanOutputTokens = meanOutputTokens
                , questionStyle = questionStyle
                , domain = domain
                , capability = capability
                , minScore = String.toFloat minScore
                , randomScore = String.toFloat randomScore
                , maxScore = String.toFloat maxScore
                , url = url
                }

        _ ->
            Err
                (CsvParseError
                    { file = "tasks.csv"
                    , line = lineNum
                    , problem = "expected 14 columns, got " ++ String.fromInt (List.length cells)
                    }
                )


parseScores : String -> ( List ValidScore, List Warning )
parseScores raw =
    let
        lines =
            String.lines raw |> List.filter (\l -> String.trim l /= "")
    in
    case lines of
        [] ->
            ( [], [ CsvParseError { file = "scores.csv", line = 0, problem = "file is empty" } ] )

        _ :: rest ->
            let
                results =
                    List.indexedMap (\i line -> parseScoreRow (i + 2) line) rest
            in
            ( List.filterMap Result.toMaybe results
            , List.filterMap
                (\r ->
                    case r of
                        Err w ->
                            Just w

                        Ok _ ->
                            Nothing
                )
                results
            )


parseScoreRow : Int -> String -> Result Warning ValidScore
parseScoreRow lineNum line =
    let
        cells =
            splitCsvLine line
    in
    case cells of
        [ modelId, taskId, scoreStr, reportedBy, source, notes ] ->
            case String.toFloat scoreStr of
                Just score ->
                    Ok
                        { modelId = modelId
                        , taskId = taskId
                        , score = score
                        , reportedBy = reportedBy
                        , source = source
                        , notes = notes
                        }

                Nothing ->
                    Err
                        (ScoreNotNumeric
                            { line = lineNum
                            , modelId = modelId
                            , taskId = taskId
                            , raw = scoreStr
                            }
                        )

        _ ->
            Err
                (CsvParseError
                    { file = "scores.csv"
                    , line = lineNum
                    , problem = "expected 6 columns, got " ++ String.fromInt (List.length cells)
                    }
                )


splitCsvLine : String -> List String
splitCsvLine line =
    String.split "," line |> List.map String.trim



-- VIEW


view : Model -> Html Msg
view model =
    case model.loading of
        Loading _ ->
            Html.div [ HA.class "p-8 text-gray-500" ] [ Html.text "Loading..." ]

        FetchError msg ->
            Html.div [ HA.class "p-8 text-red-600" ]
                [ Html.p [ HA.class "font-bold" ] [ Html.text "Failed to load data" ]
                , Html.p [ HA.class "mt-2" ] [ Html.text msg ]
                ]

        Ready leaderboard warnings hints ->
            Html.div []
                [ viewWarnings model.warningsExpanded warnings
                , viewLeaderboard model leaderboard
                , viewHints model.hintsExpanded hints
                ]


viewWarnings : Bool -> List Warning -> Html Msg
viewWarnings expanded warnings =
    if List.isEmpty warnings then
        Html.text ""

    else
        Html.div [ HA.class "mx-4 mb-4 p-3 bg-amber-50 border border-amber-300 rounded" ]
            [ Html.div
                [ HA.class "flex items-center justify-between cursor-pointer select-none"
                , Html.Events.onClick ToggleWarnings
                ]
                [ Html.span [ HA.class "font-medium text-amber-800" ]
                    [ Html.text
                        (String.fromInt (List.length warnings)
                            ++ " data warning"
                            ++ (if List.length warnings == 1 then
                                    ""

                                else
                                    "s"
                               )
                        )
                    ]
                , Html.span [ HA.class "text-amber-600" ]
                    [ Html.text
                        (if expanded then
                            "Hide"

                         else
                            "Show"
                        )
                    ]
                ]
            , if expanded then
                Html.ul [ HA.class "mt-2 text-sm text-amber-900 list-disc ml-4 space-y-1" ]
                    (List.map (\w -> Html.li [] [ Html.text (warningToString w) ]) warnings)

              else
                Html.text ""
            ]


warningToString : Warning -> String
warningToString w =
    case w of
        ScoreReferencesUnknownModel r ->
            "scores.csv: model_id \"" ++ r.modelId ++ "\" not found in models.csv"

        ScoreReferencesUnknownTask r ->
            "scores.csv: task_id \"" ++ r.taskId ++ "\" not found in tasks.csv"

        ModelHasNoScores r ->
            "Model \"" ++ r.modelName ++ "\" (" ++ r.modelId ++ ") has no scores in scores.csv"

        TaskHasNoScores r ->
            "Task \"" ++ r.taskName ++ "\" (" ++ r.taskId ++ ") has no scores in scores.csv"

        CsvParseError r ->
            r.file ++ " line " ++ String.fromInt r.line ++ ": " ++ r.problem

        ScoreNotNumeric r ->
            "scores.csv line "
                ++ String.fromInt r.line
                ++ ": score \""
                ++ r.raw
                ++ "\" for "
                ++ r.modelId
                ++ "/"
                ++ r.taskId
                ++ " is not a number"


viewHints : Bool -> List Hint -> Html Msg
viewHints expanded hints =
    if List.isEmpty hints then
        Html.text ""

    else
        Html.div [ HA.class "mx-4 mt-4 p-3 bg-blue-50 border border-blue-200 rounded" ]
            [ Html.div
                [ HA.class "flex items-center justify-between cursor-pointer select-none"
                , Html.Events.onClick ToggleHints
                ]
                [ Html.span [ HA.class "font-medium text-blue-800" ]
                    [ Html.text
                        (String.fromInt (List.length hints)
                            ++ " missing value"
                            ++ (if List.length hints == 1 then
                                    ""

                                else
                                    "s"
                               )
                        )
                    ]
                , Html.span [ HA.class "text-blue-600" ]
                    [ Html.text
                        (if expanded then
                            "Hide"

                         else
                            "Show"
                        )
                    ]
                ]
            , if expanded then
                Html.ul [ HA.class "mt-2 text-sm text-blue-900 list-disc ml-4 space-y-1" ]
                    (List.map (\h -> Html.li [] [ Html.text (hintToString h) ]) hints)

              else
                Html.text ""
            ]


hintToString : Hint -> String
hintToString h =
    case h of
        ModelMissingField r ->
            "Model \"" ++ r.modelName ++ "\" (" ++ r.modelId ++ ") is missing " ++ r.field

        TaskMissingField r ->
            "Task \"" ++ r.taskName ++ "\" (" ++ r.taskId ++ ") is missing " ++ r.field

        ScoreMissingField r ->
            "Score " ++ r.modelId ++ "/" ++ r.taskId ++ " is missing " ++ r.field


viewLeaderboard : Model -> Leaderboard -> Html Msg
viewLeaderboard model lb =
    let
        sorted =
            sortModels model.sortKey model.sortOrder lb
    in
    Html.div [ HA.class "overflow-x-auto" ]
        [ Html.table [ HA.class "w-full md:text-sm" ]
            [ viewHeader model lb.tasks
            , Html.tbody [ HA.class "border-b" ]
                (List.map (viewModelRow lb) sorted)
            ]
        ]


viewHeader : Model -> List ValidTask -> Html Msg
viewHeader model tasks =
    Html.thead [ HA.class "border-t border-b" ]
        [ Html.tr []
            (Html.th
                [ HA.class "px-2 py-1 font-medium text-left cursor-pointer"
                , Html.Events.onClick (Sort "model")
                ]
                [ Html.text "Model"
                , sortIndicator model "model"
                ]
                :: List.map (viewHeaderCell model) tasks
            )
        ]


viewHeaderCell : Model -> ValidTask -> Html Msg
viewHeaderCell model task =
    Html.th
        [ HA.class "px-2 py-1 font-medium text-right cursor-pointer"
        , Html.Events.onClick (Sort task.id)
        , HA.title (task.description ++ " (" ++ task.metric ++ ")")
        ]
        [ if String.isEmpty task.url then
            Html.text task.name

          else
            Html.a [ HA.href task.url, HA.class "underline", HA.target "_blank" ] [ Html.text task.name ]
        , sortIndicator model task.id
        ]


sortIndicator : Model -> String -> Html Msg
sortIndicator model key =
    if model.sortKey == key then
        case model.sortOrder of
            Desc ->
                Html.span [ HA.class "ml-1 text-xs" ] [ Html.text "▼" ]

            Asc ->
                Html.span [ HA.class "ml-1 text-xs" ] [ Html.text "▲" ]

    else
        Html.text ""


viewModelRow : Leaderboard -> ValidModel -> Html Msg
viewModelRow lb m =
    let
        modelScores =
            Dict.get m.id lb.scoresByModel |> Maybe.withDefault Dict.empty
    in
    Html.tr [ HA.class "hover:bg-gray-100 transition-colors" ]
        (Html.td [ HA.class "px-2 py-1 text-left" ]
            [ if String.isEmpty m.link then
                Html.text m.name

              else
                Html.a [ HA.href m.link, HA.class "underline", HA.target "_blank" ] [ Html.text m.name ]
            ]
            :: List.map
                (\task ->
                    let
                        maybeScore =
                            Dict.get task.id modelScores
                    in
                    Html.td [ HA.class "px-2 py-1 text-right font-mono" ]
                        [ case maybeScore of
                            Just s ->
                                if String.isEmpty s.source then
                                    Html.text (Round.round 1 s.score)

                                else
                                    Html.a
                                        [ HA.href s.source
                                        , HA.class "underline"
                                        , HA.target "_blank"
                                        , HA.title ("Source: " ++ s.reportedBy)
                                        ]
                                        [ Html.text (Round.round 1 s.score) ]

                            Nothing ->
                                Html.text "-"
                        ]
                )
                lb.tasks
        )



-- SORTING


sortModels : String -> Order -> Leaderboard -> List ValidModel
sortModels key order lb =
    let
        comparator =
            if key == "model" then
                \a b -> compare a.name b.name

            else
                \a b ->
                    let
                        scoreA =
                            getModelTaskScore lb a.id key |> Maybe.withDefault -1

                        scoreB =
                            getModelTaskScore lb b.id key |> Maybe.withDefault -1
                    in
                    compare scoreA scoreB

        sorted =
            List.sortWith comparator lb.models
    in
    case order of
        Desc ->
            List.reverse sorted

        Asc ->
            sorted


getModelTaskScore : Leaderboard -> String -> String -> Maybe Float
getModelTaskScore lb modelId taskId =
    Dict.get modelId lb.scoresByModel
        |> Maybe.andThen (Dict.get taskId)
        |> Maybe.map .score



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
