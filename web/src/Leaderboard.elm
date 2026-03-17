module Leaderboard exposing (..)

import Browser
import Csv
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
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
    , scoresByModel : Dict String (Dict String (List ValidScore))
    , orgColors : Dict String String
    }


type Warning
    = ScoreReferencesUnknownModel { line : Int, modelId : String }
    | ScoreReferencesUnknownTask { line : Int, taskId : String }
    | ModelHasNoScores { modelId : String, modelName : String }
    | TaskHasNoScores { taskId : String, taskName : String }
    | CsvParseError { file : String, line : Int, problem : String }
    | ScoreNotNumeric { line : Int, modelId : String, taskId : String, raw : String }
    | ModelMissingProviderScore { modelId : String, modelName : String, taskId : String, taskName : String }
    | DuplicateProviderScore { modelId : String, taskId : String }


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


type Fieldset
    = OrgsFieldset
    | ParamRangesFieldset
    | TasksFieldset
    | ModalityFieldset
    | QuestionStyleFieldset
    | DomainFieldset
    | CapabilityFieldset


allParamRanges : List ( Int, Int )
allParamRanges =
    [ ( 0, 1000 )
    , ( 1000, 4000 )
    , ( 4000, 8000 )
    , ( 8000, 12000 )
    , ( 12000, 999999999 )
    ]


paramRangeToString : ( Int, Int ) -> String
paramRangeToString ( low, high ) =
    String.fromInt low ++ "-" ++ String.fromInt high


paramRangeLabel : ( Int, Int ) -> String
paramRangeLabel ( low, high ) =
    if low == 0 then
        "<1B"

    else if high == 999999999 then
        String.fromInt (low // 1000) ++ "B+"

    else
        String.fromInt (low // 1000) ++ "-" ++ String.fromInt (high // 1000) ++ "B"


paramRangeMatch : Set String -> Maybe Float -> Bool
paramRangeMatch selected paramsM =
    case paramsM of
        Nothing ->
            Set.member "unknown" selected

        Just p ->
            List.any
                (\( low, high ) ->
                    p >= toFloat low && p < toFloat high && Set.member (paramRangeToString ( low, high )) selected
                )
                allParamRanges


type alias Model =
    { loading : LoadState
    , sortKey : String
    , sortOrder : Order
    , warningsExpanded : Bool
    , hintsExpanded : Bool
    , orgsOpen : Bool
    , orgsSelected : Set String
    , paramRangesOpen : Bool
    , paramRangesSelected : Set String
    , tasksOpen : Bool
    , tasksSelected : Set String
    , modalityOpen : Bool
    , questionStyleOpen : Bool
    , domainOpen : Bool
    , capabilityOpen : Bool
    , hoveredCell : Maybe ( String, String )
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { loading = Loading { models = Nothing, tasks = Nothing, scores = Nothing }
      , sortKey = "params"
      , sortOrder = Desc
      , warningsExpanded = False
      , hintsExpanded = False
      , orgsOpen = False
      , orgsSelected = Set.empty
      , paramRangesOpen = False
      , paramRangesSelected = Set.empty
      , tasksOpen = False
      , tasksSelected = Set.empty
      , modalityOpen = False
      , questionStyleOpen = False
      , domainOpen = False
      , capabilityOpen = False
      , hoveredCell = Nothing
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
    | ToggleFieldset Fieldset
    | ToggleOrg String
    | ToggleParamRange String
    | ToggleTask String
    | ToggleTasksByMetadata String String
    | HoverCell (Maybe ( String, String ))
    | NoOp


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

        ToggleFieldset fieldset ->
            case fieldset of
                OrgsFieldset ->
                    ( { model | orgsOpen = not model.orgsOpen }, Cmd.none )

                ParamRangesFieldset ->
                    ( { model | paramRangesOpen = not model.paramRangesOpen }, Cmd.none )

                TasksFieldset ->
                    ( { model | tasksOpen = not model.tasksOpen }, Cmd.none )

                ModalityFieldset ->
                    ( { model | modalityOpen = not model.modalityOpen }, Cmd.none )

                QuestionStyleFieldset ->
                    ( { model | questionStyleOpen = not model.questionStyleOpen }, Cmd.none )

                DomainFieldset ->
                    ( { model | domainOpen = not model.domainOpen }, Cmd.none )

                CapabilityFieldset ->
                    ( { model | capabilityOpen = not model.capabilityOpen }, Cmd.none )

        ToggleOrg org ->
            ( { model | orgsSelected = toggleSet org model.orgsSelected }, Cmd.none )

        ToggleParamRange range ->
            ( { model | paramRangesSelected = toggleSet range model.paramRangesSelected }, Cmd.none )

        ToggleTask taskId ->
            ( { model | tasksSelected = toggleSet taskId model.tasksSelected }, Cmd.none )

        ToggleTasksByMetadata field value ->
            case model.loading of
                Ready lb _ _ ->
                    let
                        matchingTaskIds =
                            lb.tasks
                                |> List.filter (\t -> List.member value (getTaskMetadataValues field t))
                                |> List.map .id

                        allSelected =
                            List.all (\id -> Set.member id model.tasksSelected) matchingTaskIds

                        newSelected =
                            if allSelected then
                                List.foldl Set.remove model.tasksSelected matchingTaskIds

                            else
                                List.foldl Set.insert model.tasksSelected matchingTaskIds
                    in
                    ( { model | tasksSelected = newSelected }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        HoverCell cell ->
            ( { model | hoveredCell = cell }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


toggleSet : comparable -> Set comparable -> Set comparable
toggleSet item set =
    if Set.member item set then
        Set.remove item set

    else
        Set.insert item set


getTaskMetadataField : String -> ValidTask -> String
getTaskMetadataField field task =
    case field of
        "modality" ->
            task.modality

        "questionStyle" ->
            task.questionStyle

        "domain" ->
            task.domain

        "capability" ->
            task.capability

        _ ->
            ""


getTaskMetadataValues : String -> ValidTask -> List String
getTaskMetadataValues field task =
    getTaskMetadataField field task
        |> String.split "+"
        |> List.map String.trim
        |> List.filter (\v -> not (String.isEmpty v))


orgPalette : List String
orgPalette =
    [ "#332288"
    , "#117733"
    , "#44AA99"
    , "#88CCEE"
    , "#DDCC77"
    , "#CCBB44"
    , "#EE6677"
    , "#CC6677"
    , "#AA4499"
    , "#882255"
    , "#E69F00"
    , "#999933"
    ]


orgColorOverrides : Dict String String
orgColorOverrides =
    Dict.fromList
        [ ( "Allen Institute for AI", "#88CCEE" )
        , ( "Google", "#CCBB44" )
        , ( "HuggingFace", "#AA4499" )
        , ( "InternVL", "#CC6677" )
        , ( "Liquid AI", "#117733" )
        , ( "Moondream", "#DDCC77" )
        , ( "OpenAI", "#E69F00" )
        , ( "Perceptron", "#332288" )
        , ( "Qwen", "#44AA99" )
        , ( "StepFun", "#EE6677" )
        , ( "Tencent AI", "#882255" )
        ]


defaultOrgColor : String
defaultOrgColor =
    "#9CA3AF"


orgPattern : String -> Maybe String
orgPattern org =
    case String.trim org of
        "Moondream" ->
            Just "repeating-linear-gradient(45deg, rgba(17,24,39,0.34) 0 1px, transparent 1px 3px)"

        "Google" ->
            Just "repeating-linear-gradient(0deg, rgba(17,24,39,0.28) 0 1px, transparent 1px 3px)"

        "InternVL" ->
            Just "repeating-linear-gradient(135deg, rgba(255,255,255,0.55) 0 1px, transparent 1px 3px)"

        "StepFun" ->
            Just "radial-gradient(rgba(255,255,255,0.85) 0.7px, transparent 0.8px)"

        _ ->
            Nothing


orgColor : Dict String String -> String -> String
orgColor orgColors org =
    let
        trimmed =
            String.trim org
    in
    if String.isEmpty trimmed then
        defaultOrgColor

    else
        Dict.get trimmed orgColors
            |> Maybe.withDefault defaultOrgColor


orgSwatchAttrs : Dict String String -> String -> List (Html.Attribute msg)
orgSwatchAttrs orgColors org =
    let
        baseColor =
            orgColor orgColors org

        patternStyles =
            case orgPattern org of
                Just pattern ->
                    [ HA.style "background-image" pattern
                    , HA.style "background-size" "4px 4px"
                    ]

                Nothing ->
                    []

        borderColor =
            case String.trim org of
                "Moondream" ->
                    "rgba(17,24,39,0.28)"

                "Google" ->
                    "rgba(17,24,39,0.20)"

                _ ->
                    "rgba(255,255,255,0.0)"
    in
    [ HA.style "background-color" baseColor
    , HA.style "border" ("1px solid " ++ borderColor)
    , HA.style "box-sizing" "border-box"
    ]
        ++ patternStyles


providerScore : List ValidScore -> String -> Maybe ValidScore
providerScore scores org =
    scores
        |> List.filter (\s -> String.toLower (String.trim s.reportedBy) == String.toLower (String.trim org))
        |> List.head


getModelOrg : Leaderboard -> String -> String
getModelOrg lb modelId =
    lb.models
        |> List.filter (\m -> m.id == modelId)
        |> List.head
        |> Maybe.map .org
        |> Maybe.withDefault ""


tryFinalize : { models : Maybe String, tasks : Maybe String, scores : Maybe String } -> Model -> ( Model, Cmd Msg )
tryFinalize state model =
    case ( state.models, state.tasks, state.scores ) of
        ( Just m, Just t, Just s ) ->
            let
                ( leaderboard, warnings, hints ) =
                    parseAndValidate m t s

                orgs =
                    Set.fromList (List.map .org leaderboard.models)

                ranges =
                    allParamRanges
                        |> List.filter (\( _, high ) -> high <= 12000)
                        |> List.map paramRangeToString

                defaultHidden =
                    Set.fromList [ "mme", "zerobench" ]

                tasks =
                    leaderboard.tasks
                        |> List.map .id
                        |> List.filter (\id -> not (Set.member id defaultHidden))
                        |> Set.fromList
            in
            ( { model
                | loading = Ready leaderboard warnings hints
                , orgsSelected = orgs
                , paramRangesSelected = Set.fromList ranges
                , tasksSelected = tasks
              }
            , Cmd.none
            )

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

        -- Build nested dict: model_id -> (task_id -> [scores])
        scoresByModel =
            List.foldl
                (\s acc ->
                    let
                        inner =
                            Dict.get s.modelId acc |> Maybe.withDefault Dict.empty

                        existing =
                            Dict.get s.taskId inner |> Maybe.withDefault []
                    in
                    Dict.insert s.modelId (Dict.insert s.taskId (existing ++ [ s ]) inner) acc
                )
                Dict.empty
                validScores

        -- Keep known organizations pinned to explicit colors.
        -- Unknown organizations get deterministic fallback colors without
        -- reshuffling the existing assignments.
        orgColors =
            let
                modelOrgs =
                    models
                        |> List.map (.org >> String.trim)
                        |> List.filter (\org -> not (String.isEmpty org))

                reporterOrgs =
                    List.foldl
                        (\s ( seen, acc ) ->
                            let
                                rb =
                                    String.trim s.reportedBy
                            in
                            if String.isEmpty rb || Set.member rb seen then
                                ( seen, acc )

                            else
                                ( Set.insert rb seen, acc ++ [ rb ] )
                        )
                        ( Set.empty, [] )
                        validScores
                        |> Tuple.second

                uniqueOrgs =
                    List.foldl
                        (\org ( seen, acc ) ->
                            if Set.member org seen then
                                ( seen, acc )

                            else
                                ( Set.insert org seen, acc ++ [ org ] )
                        )
                        ( Set.empty, [] )
                        (modelOrgs ++ reporterOrgs)
                        |> Tuple.second

                usedColors =
                    orgColorOverrides
                        |> Dict.values
                        |> Set.fromList

                fallbackPalette =
                    orgPalette
                        |> List.filter (\color -> not (Set.member color usedColors))

                paletteLen =
                    List.length fallbackPalette
            in
            List.indexedMap
                (\i org ->
                    ( org
                    , Dict.get org orgColorOverrides
                        |> Maybe.withDefault
                            (if paletteLen == 0 then
                                defaultOrgColor

                             else
                                List.drop (modBy paletteLen i) fallbackPalette
                                    |> List.head
                                    |> Maybe.withDefault defaultOrgColor
                            )
                    )
                )
                uniqueOrgs
                |> Dict.fromList

        -- Provider score validation
        modelOrgDict =
            Dict.fromList (List.map (\m -> ( m.id, m.org )) models)

        modelNameDict =
            Dict.fromList (List.map (\m -> ( m.id, m.name )) models)

        taskNameDict =
            Dict.fromList (List.map (\t -> ( t.id, t.name )) tasks)

        providerWarnings =
            Dict.toList scoresByModel
                |> List.concatMap
                    (\( modelId, taskScores ) ->
                        let
                            org =
                                Dict.get modelId modelOrgDict |> Maybe.withDefault ""

                            modelName =
                                Dict.get modelId modelNameDict |> Maybe.withDefault modelId
                        in
                        Dict.toList taskScores
                            |> List.concatMap
                                (\( taskId, scoreList ) ->
                                    let
                                        taskName =
                                            Dict.get taskId taskNameDict |> Maybe.withDefault taskId

                                        providerMatches =
                                            List.filter
                                                (\s ->
                                                    String.toLower (String.trim s.reportedBy)
                                                        == String.toLower (String.trim org)
                                                )
                                                scoreList
                                    in
                                    (if List.length scoreList > 1 && List.isEmpty providerMatches && not (String.isEmpty org) then
                                        [ ModelMissingProviderScore { modelId = modelId, modelName = modelName, taskId = taskId, taskName = taskName } ]

                                     else
                                        []
                                    )
                                        ++ (if List.length providerMatches > 1 then
                                                [ DuplicateProviderScore { modelId = modelId, taskId = taskId } ]

                                            else
                                                []
                                           )
                                )
                    )
                |> dedup warningKey

        allWarnings =
            modelWarnings
                ++ taskWarnings
                ++ scoreWarnings
                ++ danglingModelWarnings
                ++ danglingTaskWarnings
                ++ noScoreModelWarnings
                ++ noScoreTaskWarnings
                ++ providerWarnings

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
    ( { models = models, tasks = tasks, scoresByModel = scoresByModel, orgColors = orgColors }
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

        ModelMissingProviderScore r ->
            "missing-provider:" ++ r.modelId ++ ":" ++ r.taskId

        DuplicateProviderScore r ->
            "dup-provider:" ++ r.modelId ++ ":" ++ r.taskId


parseCsv : String -> String -> (Int -> List String -> Result Warning a) -> ( List a, List Warning )
parseCsv fileName raw parseRow =
    let
        csv =
            Csv.parse raw

        results =
            List.indexedMap (\i row -> parseRow (i + 2) (List.map String.trim row)) csv.records
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


parseModels : String -> ( List ValidModel, List Warning )
parseModels raw =
    parseCsv "models.csv" raw parseModelRow


parseModelRow : Int -> List String -> Result Warning ValidModel
parseModelRow lineNum cells =
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
    parseCsv "tasks.csv" raw parseTaskRow


parseTaskRow : Int -> List String -> Result Warning ValidTask
parseTaskRow lineNum cells =
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
    parseCsv "scores.csv" raw parseScoreRow


parseScoreRow : Int -> List String -> Result Warning ValidScore
parseScoreRow lineNum cells =
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
                [ viewLeaderboard model leaderboard
                , viewWarnings model.warningsExpanded warnings
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

        ModelMissingProviderScore r ->
            "No provider score for \"" ++ r.modelName ++ "\" on task \"" ++ r.taskName ++ "\" (reported_by doesn't match org)"

        DuplicateProviderScore r ->
            "Duplicate provider score for " ++ r.modelId ++ " on " ++ r.taskId ++ " (using first)"


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


viewCheckbox : Bool -> Msg -> String -> Html Msg
viewCheckbox checked msg label =
    Html.label [ HA.class "flex items-center gap-1.5 cursor-pointer select-none whitespace-nowrap" ]
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.checked checked
            , Html.Events.onClick msg
            , HA.class "cursor-pointer"
            ]
            []
        , Html.text label
        ]


viewOrgCheckbox : Dict String String -> Bool -> Msg -> String -> Html Msg
viewOrgCheckbox orgColors checked msg org =
    Html.label [ HA.class "flex items-center gap-1.5 cursor-pointer select-none whitespace-nowrap" ]
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.checked checked
            , Html.Events.onClick msg
            , HA.class "cursor-pointer"
            ]
            []
        , Html.span
            ([ HA.class "inline-block w-3 h-3 rounded-full flex-shrink-0" ]
                ++ orgSwatchAttrs orgColors org
            )
            []
        , Html.text org
        ]


viewFieldset : String -> Bool -> Msg -> Int -> Int -> List (Html Msg) -> Html Msg
viewFieldset title isOpen toggleMsg selected total children =
    Html.div [ HA.class "inline-block" ]
        [ Html.button
            [ HA.class "px-3 py-1.5 text-sm border rounded cursor-pointer select-none hover:bg-gray-50 flex items-center gap-1"
            , Html.Events.onClick toggleMsg
            ]
            [ Html.text
                (title
                    ++ " ("
                    ++ String.fromInt selected
                    ++ "/"
                    ++ String.fromInt total
                    ++ ") "
                    ++ (if isOpen then
                            "▲"

                        else
                            "▼"
                       )
                )
            ]
        , if isOpen then
            Html.div [ HA.class "absolute z-10 mt-1 p-2 bg-white border rounded shadow-lg flex flex-col gap-1 text-sm" ]
                children

          else
            Html.text ""
        ]


uniqueValues : (ValidTask -> String) -> List ValidTask -> List String
uniqueValues accessor tasks =
    tasks
        |> List.concatMap (\t -> String.split "+" (accessor t))
        |> List.map String.trim
        |> List.filter (\v -> not (String.isEmpty v))
        |> Set.fromList
        |> Set.toList
        |> List.sort


metadataAllSelected : String -> String -> List ValidTask -> Set String -> Bool
metadataAllSelected field value tasks tasksSelected =
    tasks
        |> List.filter (\t -> List.member value (getTaskMetadataValues field t))
        |> List.all (\t -> Set.member t.id tasksSelected)


viewFilters : Model -> Leaderboard -> Html Msg
viewFilters model lb =
    let
        allOrgs =
            List.map .org lb.models |> Set.fromList |> Set.toList |> List.sort

        hasUnknownSize =
            List.any (\m -> m.paramsM == Nothing) lb.models

        totalRanges =
            List.length allParamRanges
                + (if hasUnknownSize then
                    1

                   else
                    0
                  )

        selectedRanges =
            Set.size model.paramRangesSelected

        modalities =
            uniqueValues .modality lb.tasks

        questionStyles =
            uniqueValues .questionStyle lb.tasks

        domains =
            uniqueValues .domain lb.tasks

        capabilities =
            uniqueValues .capability lb.tasks
    in
    Html.div [ HA.class "mx-4 mb-3 flex flex-wrap gap-2 items-start" ]
        [ viewFieldset "Orgs"
            model.orgsOpen
            (ToggleFieldset OrgsFieldset)
            (Set.size model.orgsSelected)
            (List.length allOrgs)
            (List.map
                (\org ->
                    viewOrgCheckbox lb.orgColors (Set.member org model.orgsSelected) (ToggleOrg org) org
                )
                allOrgs
            )
        , viewFieldset "Size"
            model.paramRangesOpen
            (ToggleFieldset ParamRangesFieldset)
            selectedRanges
            totalRanges
            (List.map
                (\range ->
                    viewCheckbox
                        (Set.member (paramRangeToString range) model.paramRangesSelected)
                        (ToggleParamRange (paramRangeToString range))
                        (paramRangeLabel range)
                )
                allParamRanges
                ++ (if hasUnknownSize then
                        [ viewCheckbox
                            (Set.member "unknown" model.paramRangesSelected)
                            (ToggleParamRange "unknown")
                            "Unknown"
                        ]

                    else
                        []
                   )
            )
        , viewFieldset "Tasks"
            model.tasksOpen
            (ToggleFieldset TasksFieldset)
            (Set.size model.tasksSelected)
            (List.length lb.tasks)
            (List.map
                (\task ->
                    viewCheckbox (Set.member task.id model.tasksSelected) (ToggleTask task.id) task.name
                )
                lb.tasks
            )
        , viewFieldset "Style"
            model.questionStyleOpen
            (ToggleFieldset QuestionStyleFieldset)
            (List.length (List.filter (\v -> metadataAllSelected "questionStyle" v lb.tasks model.tasksSelected) questionStyles))
            (List.length questionStyles)
            (List.map
                (\v ->
                    viewCheckbox
                        (metadataAllSelected "questionStyle" v lb.tasks model.tasksSelected)
                        (ToggleTasksByMetadata "questionStyle" v)
                        v
                )
                questionStyles
            )
        , viewFieldset "Domain"
            model.domainOpen
            (ToggleFieldset DomainFieldset)
            (List.length (List.filter (\v -> metadataAllSelected "domain" v lb.tasks model.tasksSelected) domains))
            (List.length domains)
            (List.map
                (\v ->
                    viewCheckbox
                        (metadataAllSelected "domain" v lb.tasks model.tasksSelected)
                        (ToggleTasksByMetadata "domain" v)
                        v
                )
                domains
            )
        , viewFieldset "Modality"
            model.modalityOpen
            (ToggleFieldset ModalityFieldset)
            (List.length (List.filter (\v -> metadataAllSelected "modality" v lb.tasks model.tasksSelected) modalities))
            (List.length modalities)
            (List.map
                (\v ->
                    viewCheckbox
                        (metadataAllSelected "modality" v lb.tasks model.tasksSelected)
                        (ToggleTasksByMetadata "modality" v)
                        v
                )
                modalities
            )
        , viewFieldset "Capability"
            model.capabilityOpen
            (ToggleFieldset CapabilityFieldset)
            (List.length (List.filter (\v -> metadataAllSelected "capability" v lb.tasks model.tasksSelected) capabilities))
            (List.length capabilities)
            (List.map
                (\v ->
                    viewCheckbox
                        (metadataAllSelected "capability" v lb.tasks model.tasksSelected)
                        (ToggleTasksByMetadata "capability" v)
                        v
                )
                capabilities
            )
        ]


viewLeaderboard : Model -> Leaderboard -> Html Msg
viewLeaderboard model lb =
    let
        filteredModels =
            lb.models
                |> List.filter (\m -> Set.member m.org model.orgsSelected)
                |> List.filter (\m -> paramRangeMatch model.paramRangesSelected m.paramsM)

        filteredTasks =
            lb.tasks
                |> List.filter (\t -> Set.member t.id model.tasksSelected)

        filteredLb =
            { lb | models = filteredModels, tasks = filteredTasks }

        sorted =
            sortModels model.sortKey model.sortOrder filteredLb
    in
    Html.div []
        [ viewFilters model lb
        , Html.div [ HA.class "overflow-x-auto" ]
            [ Html.table [ HA.class "w-full text-sm" ]
                [ viewHeader model filteredTasks
                , Html.tbody [ HA.class "border-b" ]
                    (List.map (viewModelRow model filteredLb) sorted)
                ]
            ]
        ]


viewHeader : Model -> List ValidTask -> Html Msg
viewHeader model tasks =
    Html.thead [ HA.class "border-t border-b" ]
        [ Html.tr []
            (Html.th
                [ HA.class (sortHeaderClass model "model" "px-2 py-1 text-left")
                , Html.Events.onClick (Sort "model")
                ]
                [ sortIndicator model "model"
                , sortHeaderLabel model "model" "Model"
                ]
                :: Html.th
                    [ HA.class (sortHeaderClass model "params" "px-2 py-1 text-right")
                    , Html.Events.onClick (Sort "params")
                    ]
                    [ sortIndicator model "params"
                    , sortHeaderLabel model "params" "Params"
                    ]
                :: Html.th
                    [ HA.class (sortHeaderClass model "mean" "px-2 py-1 text-right")
                    , Html.Events.onClick (Sort "mean")
                    , HA.title "Mean score across selected tasks (only tasks with a score)"
                    ]
                    [ sortIndicator model "mean"
                    , sortHeaderLabel model "mean" "Mean"
                    ]
                :: List.map (viewHeaderCell model) tasks
            )
        ]


viewHeaderCell : Model -> ValidTask -> Html Msg
viewHeaderCell model task =
    Html.th
        [ HA.class (sortHeaderClass model task.id "pl-0 pr-2 py-1 text-right")
        , Html.Events.onClick (Sort task.id)
        , HA.title (task.description ++ " (" ++ task.metric ++ ")")
        ]
        [ sortIndicator model task.id
        , sortHeaderLabel model task.id task.name
        , if String.isEmpty task.url then
            Html.text ""

          else
            Html.a
                [ HA.href task.url
                , HA.target "_blank"
                , HA.class "ml-1 text-blue-600 underline"
                , Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( NoOp, False ))
                ]
                [ Html.text "\u{2197}" ]
        ]


sortIndicator : Model -> String -> Html Msg
sortIndicator model key =
    if model.sortKey == key then
        case model.sortOrder of
            Desc ->
                Html.span [ HA.class "mr-1 text-xs" ] [ Html.text "▼" ]

            Asc ->
                Html.span [ HA.class "mr-1 text-xs" ] [ Html.text "▲" ]

    else
        Html.text ""


sortHeaderClass : Model -> String -> String -> String
sortHeaderClass model key baseClass =
    baseClass
        ++ " cursor-pointer hover:bg-gray-100 transition-colors"
        ++ (if model.sortKey == key then
                " font-semibold"

            else
                " font-medium"
           )


sortHeaderLabel : Model -> String -> String -> Html Msg
sortHeaderLabel model key label =
    Html.span
        [ HA.class
            (if model.sortKey == key then
                "underline decoration-2 underline-offset-4"

             else
                ""
            )
        ]
        [ Html.text label ]


viewModelRow : Model -> Leaderboard -> ValidModel -> Html Msg
viewModelRow appModel lb m =
    let
        modelScores =
            Dict.get m.id lb.scoresByModel |> Maybe.withDefault Dict.empty
    in
    Html.tr [ HA.class "hover:bg-gray-100 transition-colors" ]
        (let
            orgColorClass =
                orgColor lb.orgColors m.org

            orgDot =
                Html.span
                    ([ HA.class "inline-block w-3 h-3 rounded-full mr-1.5 flex-shrink-0" ]
                        ++ orgSwatchAttrs lb.orgColors m.org
                    )
                    []

            nameEl =
                if String.isEmpty m.link then
                    Html.text m.name

                else
                    Html.a [ HA.href m.link, HA.class "underline", HA.target "_blank" ] [ Html.text m.name ]
         in
         Html.td [ HA.class "px-2 py-1 text-left" ]
            [ Html.span [ HA.class "flex items-center" ]
                [ orgDot, nameEl ]
            ]
            :: Html.td [ HA.class "px-2 py-1 text-right font-mono text-gray-500" ]
                [ Html.text (formatParams m.paramsM) ]
            :: Html.td [ HA.class "px-2 py-1 text-right font-mono font-medium" ]
                [ case meanScore lb m.id of
                    Just avg ->
                        Html.text (Round.round 1 avg)

                    Nothing ->
                        Html.text "-"
                ]
            :: List.map
                (\task ->
                    let
                        scoreList =
                            Dict.get task.id modelScores |> Maybe.withDefault []

                        displayScore =
                            case providerScore scoreList m.org of
                                Just ps ->
                                    Just ps

                                Nothing ->
                                    -- No provider match: use highest score
                                    scoreList
                                        |> List.sortBy (\s -> negate s.score)
                                        |> List.head
                    in
                    viewScoreCell appModel lb task m scoreList displayScore
                )
                lb.tasks
        )


viewScoreCell : Model -> Leaderboard -> ValidTask -> ValidModel -> List ValidScore -> Maybe ValidScore -> Html Msg
viewScoreCell appModel lb task m scoreList displayScore =
    let
        hasMultiple =
            List.length scoreList >= 2

        hasDisagreement =
            if not hasMultiple then
                False

            else
                let
                    allScores =
                        List.map .score scoreList

                    minVal =
                        List.minimum allScores |> Maybe.withDefault 0

                    maxVal =
                        List.maximum allScores |> Maybe.withDefault 0

                    spread =
                        maxVal - minVal

                    threshold =
                        case ( task.minScore, task.maxScore ) of
                            ( Just tMin, Just tMax ) ->
                                0.02 * (tMax - tMin)

                            _ ->
                                0
                in
                spread > threshold

        cellClass =
            "px-2 py-1 text-right font-mono"
                ++ (if hasDisagreement then
                        " bg-amber-50"

                    else
                        ""
                   )
                ++ (if hasMultiple then
                        " relative cursor-help"

                    else
                        ""
                   )

        hoverAttrs =
            if hasMultiple then
                [ Html.Events.onMouseEnter (HoverCell (Just ( m.id, task.id )))
                , Html.Events.onMouseLeave (HoverCell Nothing)
                ]

            else
                []

        isHovered =
            appModel.hoveredCell == Just ( m.id, task.id )
    in
    Html.td (HA.class cellClass :: hoverAttrs)
        ([ case displayScore of
            Just s ->
                Html.div [ HA.class "flex items-center justify-end gap-1" ]
                    [ if hasMultiple then
                        viewDotGrid lb.orgColors scoreList

                      else
                        Html.text ""
                    , Html.span []
                        [ Html.text (Round.round 1 s.score) ]
                    ]

            Nothing ->
                Html.text "-"
         ]
            ++ (if isHovered then
                    [ viewScoreTooltip lb.orgColors m.org ( m.id, task.id ) scoreList ]

                else
                    []
               )
        )


viewDotGrid : Dict String String -> List ValidScore -> Html Msg
viewDotGrid orgColors scores =
    let
        dots =
            scores
                |> List.sortBy (\s -> negate s.score)
                |> List.map
                (\s ->
                    Html.span
                        ([ HA.class "inline-block w-3 h-3 rounded-full"
                         , HA.title (String.trim s.reportedBy ++ ": " ++ Round.round 1 s.score)
                         ]
                            ++ orgSwatchAttrs orgColors s.reportedBy
                        )
                        []
                )

        -- Arrange in columns of up to 2
        col1 =
            List.take 2 dots

        col2 =
            List.drop 2 dots |> List.take 2
    in
    Html.div [ HA.class "inline-flex gap-0.5" ]
        (Html.div [ HA.class "flex flex-col gap-0.5" ] col1
            :: (if List.isEmpty col2 then
                    []

                else
                    [ Html.div [ HA.class "flex flex-col gap-0.5" ] col2 ]
               )
        )



viewScoreTooltip : Dict String String -> String -> ( String, String ) -> List ValidScore -> Html Msg
viewScoreTooltip orgColors modelOrg cell scoreList =
    let
        sorted =
            List.sortBy (\s -> negate s.score) scoreList

        maybeProviderVal =
            providerScore scoreList modelOrg
                |> Maybe.map .score
    in
    Html.div
        [ HA.class "absolute z-10 bg-white border border-gray-300 rounded shadow-lg p-2 text-sm whitespace-nowrap"
        , HA.style "bottom" "100%"
        , HA.style "right" "0"
        , HA.style "padding-bottom" "8px"
        , HA.style "margin-bottom" "-4px"
        , Html.Events.onMouseEnter (HoverCell (Just cell))
        , Html.Events.onMouseLeave (HoverCell Nothing)
        ]
        [ Html.table [ HA.class "text-left" ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [ HA.class "pr-3 font-medium" ] [ Html.text "Reporter" ]
                    , Html.th [ HA.class "pr-3 font-medium text-right" ] [ Html.text "Score" ]
                    , Html.th [ HA.class "pr-3 font-medium text-right" ] [ Html.text "\u{0394}" ]
                    , Html.th [ HA.class "font-medium" ] [ Html.text "Notes" ]
                    ]
                ]
            , Html.tbody []
                (List.map
                    (\s ->
                        let
                            rb =
                                String.trim s.reportedBy

                            colorClass =
                                orgColor orgColors rb

                            name =
                                if String.isEmpty rb then
                                    "unknown"

                                else
                                    rb

                            isProvider =
                                String.toLower (String.trim s.reportedBy)
                                    == String.toLower (String.trim modelOrg)

                            dot =
                                Html.span
                                    ([ HA.class "inline-block w-3 h-3 rounded-full mr-1.5" ]
                                        ++ orgSwatchAttrs orgColors rb
                                    )
                                    []

                            deltaCell =
                                case maybeProviderVal of
                                    Just pv ->
                                        if isProvider then
                                            Html.td [ HA.class "pr-3 text-right font-mono text-gray-300" ]
                                                [ Html.text "-" ]

                                        else
                                            let
                                                d =
                                                    s.score - pv

                                                sign =
                                                    if d > 0 then
                                                        "+"

                                                    else
                                                        ""

                                                color =
                                                    if d > 0 then
                                                        "text-green-600"

                                                    else if d < 0 then
                                                        "text-red-600"

                                                    else
                                                        "text-gray-400"
                                            in
                                            Html.td [ HA.class ("pr-3 text-right font-mono " ++ color) ]
                                                [ Html.text (sign ++ Round.round 1 d) ]

                                    Nothing ->
                                        Html.td [ HA.class "pr-3 text-right font-mono text-gray-300" ]
                                            [ Html.text "-" ]
                        in
                        Html.tr []
                            [ Html.td [ HA.class "pr-3" ]
                                [ Html.span [ HA.class "flex items-center" ]
                                    [ dot
                                    , if String.isEmpty s.source then
                                        Html.text name

                                      else
                                        Html.a
                                            [ HA.href s.source
                                            , HA.target "_blank"
                                            , HA.class "underline"
                                            ]
                                            [ Html.text name ]
                                    ]
                                ]
                            , Html.td [ HA.class "pr-3 text-right font-mono" ]
                                [ Html.text (Round.round 1 s.score) ]
                            , deltaCell
                            , Html.td [ HA.class "text-gray-500" ]
                                [ Html.text s.notes ]
                            ]
                    )
                    sorted
                )
            ]
        ]


formatParams : Maybe Float -> String
formatParams paramsM =
    case paramsM of
        Nothing ->
            "-"

        Just p ->
            if p >= 1000000 then
                -- Trillions
                let
                    t =
                        p / 1000000
                in
                if t >= 100 then
                    Round.round 0 t ++ "T"

                else if t >= 10 then
                    Round.round 0 t ++ "T"

                else
                    Round.round 1 t ++ "T"

            else if p >= 1000 then
                -- Billions
                let
                    b =
                        p / 1000
                in
                if b >= 100 then
                    Round.round 0 b ++ "B"

                else if b >= 10 then
                    Round.round 0 b ++ "B"

                else
                    Round.round 1 b ++ "B"

            else
                -- Millions
                if p >= 100 then
                    Round.round 0 p ++ "M"

                else if p >= 10 then
                    Round.round 0 p ++ "M"

                else
                    Round.round 1 p ++ "M"


-- SORTING


meanScore : Leaderboard -> String -> Maybe Float
meanScore lb modelId =
    let
        scores =
            lb.tasks
                |> List.filterMap (\t -> getModelTaskScore lb modelId t.id)
    in
    if List.length scores == List.length lb.tasks && not (List.isEmpty scores) then
        Just (List.sum scores / toFloat (List.length scores))

    else
        Nothing


sortModels : String -> Order -> Leaderboard -> List ValidModel
sortModels key order lb =
    let
        comparator =
            if key == "model" then
                \a b -> compare a.name b.name

            else if key == "params" then
                \a b ->
                    compare
                        (a.paramsM |> Maybe.withDefault -1)
                        (b.paramsM |> Maybe.withDefault -1)

            else if key == "mean" then
                \a b ->
                    compare
                        (meanScore lb a.id |> Maybe.withDefault -1)
                        (meanScore lb b.id |> Maybe.withDefault -1)

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
    let
        org =
            getModelOrg lb modelId
    in
    Dict.get modelId lb.scoresByModel
        |> Maybe.andThen (Dict.get taskId)
        |> Maybe.andThen
            (\scores ->
                case providerScore scores org of
                    Just ps ->
                        Just ps.score

                    Nothing ->
                        -- Fallback: highest non-provider score
                        scores
                            |> List.sortBy (\s -> negate s.score)
                            |> List.head
                            |> Maybe.map .score
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
