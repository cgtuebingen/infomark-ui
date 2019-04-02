{-
   This is the login site. Here, tutors and TaskForStudents should get
   an overview about what to do next.
       - Users should see:
           - All enrolled courses
       - TaskForStudents should see (per course):
           - An overview of their points for each sheet
           - The total amount of points acquired
           - The needed amount of points to pass the course
           - What tasks are missing
       - Tutors should see (per course):
           - The point distribution per exercise sheet of the course or group?
           - The tasks which are not done grading
-}


module Pages.Dashboard exposing (Model, Msg(..), init, update, view)

import Api.Data.AccountEnrollment exposing (AccountEnrollment)
import Api.Data.Course exposing (Course)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.Group exposing (Group)
import Api.Data.MissingGrade exposing (MissingGrade)
import Api.Data.MissingTask exposing (MissingTask)
import Api.Data.Sheet exposing (Sheet)
import Api.Data.Task exposing (Task)
import Api.Request.Account as AccountRequests
import Api.Request.Courses as CourseRequests
import Api.Request.Sheet as SheetRequests
import Api.Request.Task as TaskRequests
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements as CE
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import Maybe.Extra exposing (isJust)
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import Set exposing (Set)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Utils.Utils exposing (perform, tupleExtend, unzipTripple)


type Msg
    = NavigateTo Route
    | GetCourses (WebData (List Course))
    | GetAccountEnrollments (WebData (List AccountEnrollment))
    | GetMissingItem Int TodoRequest
    | GetSheet (WebData Sheet)
    | GetTasks (WebData (List Task))
    | GetOwnGroup Int (WebData (List Group))


type TodoRequest
    = TaskRequest (WebData (List MissingTask))
    | GradeRequest (WebData (List MissingGrade))


type TodoItems
    = TaskTodo (List MissingTask)
    | GradeTodo (List MissingGrade)


type alias FusedEnrollment =
    { course : Course
    , role : CourseRole
    , missingItems : Maybe TodoItems
    , groups : Maybe (List Group)
    }


type alias Model =
    { getCourses : WebData (List Course)
    , getAccountEnrollments : WebData (List AccountEnrollment)
    , fusedEnrollmentDict : Dict Int FusedEnrollment
    , sheetDict : Dict Int Sheet
    , taskDict : Dict Int Task
    }


init : SharedState -> ( Model, Cmd Msg )
init sharedState =
    let
        startRequests =
            Cmd.batch
                [ CourseRequests.coursesGet GetCourses
                , AccountRequests.accountEnrollmentGet GetAccountEnrollments
                ]
    in
    ( { getCourses = Loading
      , getAccountEnrollments = Loading
      , fusedEnrollmentDict = Dict.empty
      , sheetDict = Dict.empty
      , taskDict = Dict.empty
      }
    , case ( sharedState.userMail, sharedState.role ) of
        -- Check if we are a global admin
        ( Just _, Just role ) ->
            if role.root then
                -- In this case the dashboard has nothing to show..
                perform <| NavigateTo CoursesRoute

            else
                -- We are a mere mortal. Start the default requests to populate the view
                startRequests

        ( _, _ ) ->
            {- This one includes even lost userMail. Here, a request is started
               which will fail and trigger a login refresh dialog. If no usermail is
               left in the persistant or sharedstate then the user is redirected to
               the login form
            -}
            startRequests
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        GetCourses response ->
            fillFusedEnrollmentAndDecideRequests
                { model | getCourses = response }

        GetAccountEnrollments response ->
            fillFusedEnrollmentAndDecideRequests
                { model | getAccountEnrollments = response }

        GetMissingItem courseId response ->
            tupleExtend
                (updateFusedEnrollmentWithTodosAndNextRequests model courseId response)
                NoUpdate

        GetSheet response ->
            ( updateSheetsDict model response
            , Cmd.none
            , NoUpdate
            )

        GetTasks response ->
            ( updateTasksDict model response
            , Cmd.none
            , NoUpdate
            )

        GetOwnGroup courseId response ->
            updateFusedEnrollmentWithGroups model courseId response


view : SharedState -> Model -> Html Msg
view sharedState model =
    CE.pageContainer <|
        [ CE.normalPage <|
            [ CE.rRowHeader "Dashboard"
            ]
                ++ (case
                        ( Dict.isEmpty model.fusedEnrollmentDict
                        , model.getAccountEnrollments
                        , model.getCourses
                        )
                    of
                        ( True, Success _, Success _ ) ->
                            [ CE.rRow <|
                                CE.r1Column
                                    [ span [ Styles.textStyle ]
                                        [ text "You are not enrolled in a course. Start by enrolling into a course:" ]
                                    ]
                            , CE.rRowButton <|
                                CE.PbbButton <|
                                    CE.PbbActive "Start enrolling" <|
                                        NavigateTo CoursesRoute
                            ]

                        ( False, Success _, Success _ ) ->
                            model.fusedEnrollmentDict
                                |> Dict.values
                                |> List.map (viewCourseWithTodos sharedState model)

                        ( _, _, _ ) ->
                            [ text "" ]
                   )
        ]


viewCourseWithTodos : SharedState -> Model -> FusedEnrollment -> Html Msg
viewCourseWithTodos sharedState model fusedEnrollment =
    let
        allDoneHtml =
            [ CE.rRowExtraSpacing <|
                [ span [ Styles.listHeadingStyle ]
                    [ text "Nothing left to do in this course for you. Good job. Wait till the next exercise sheet is uploaded." ]
                ]
            ]

        dateAndTimeShort =
            \t ->
                DF.shortDateFormatter sharedState t
                    ++ " - "
                    ++ DF.timeFormatter sharedState t
    in
    CE.rContainer <|
        CE.rRowHeaderActionButtons
            fusedEnrollment.course.name
            Styles.headerStyle
            [ ( "Show", NavigateTo <| CourseDetailRoute fusedEnrollment.course.id, Styles.buttonGreyStyle )
            ]
            :: (case fusedEnrollment.missingItems of
                    Just (TaskTodo []) ->
                        allDoneHtml

                    Just (GradeTodo []) ->
                        allDoneHtml

                    Nothing ->
                        allDoneHtml

                    Just (TaskTodo missingTasks) ->
                        let
                            firstDueDate =
                                missingTasks
                                    |> List.map (\mt -> mt.sheet_id)
                                    |> List.map (\sid -> Dict.get sid model.sheetDict)
                                    |> List.filterMap identity
                                    -- Remove nothings
                                    |> List.map (\sheet -> sheet.due_at)
                                    |> List.sortBy (\t -> Time.posixToMillis t)
                                    |> List.head

                            taskWithSheet =
                                missingTasks
                                    |> List.map (\mt -> ( mt, Dict.get mt.sheet_id model.sheetDict ))
                                    |> List.filterMap
                                        (\( mt, sheet ) ->
                                            case sheet of
                                                Just s ->
                                                    Just ( mt, s )

                                                Nothing ->
                                                    Nothing
                                        )
                        in
                        (CE.rRowExtraSpacing <|
                            [ span [ Styles.listHeadingStyle ]
                                [ text <|
                                    "You have missing tasks to complete"
                                        ++ (case firstDueDate of
                                                Just date ->
                                                    ". The first is due on "
                                                        ++ DF.shortDateFormatter sharedState date
                                                        ++ " at "
                                                        ++ DF.timeFormatter sharedState date
                                                        ++ ":"

                                                Nothing ->
                                                    ": "
                                           )
                                ]
                            ]
                        )
                            :: (taskWithSheet
                                    |> List.map
                                        (\( mt, sheet ) ->
                                            CE.rRowHeaderActionButtons
                                                (mt.task.name ++ "  from " ++ sheet.name ++ " due on " ++ dateAndTimeShort sheet.due_at)
                                                Styles.listHeadingStyle
                                                [ ( "Submit", NavigateTo <| SheetDetailRoute fusedEnrollment.course.id sheet.id, Styles.buttonGreenStyle )
                                                ]
                                        )
                               )

                    Just (GradeTodo missingGrades) ->
                        let
                            sheettaskWithGrades =
                                missingGrades
                                    |> List.foldl
                                        -- This gets tricky. Maybe not the nicest code around...
                                        (\mg d ->
                                            -- We're gonna create a dict with the sheet.id as key, which
                                            -- Contains another dict with the task.id as the key..
                                            Dict.update mg.sheet_id
                                                (\maybeEntry ->
                                                    case maybeEntry of
                                                        -- Check if we already have values in the key
                                                        Just entry ->
                                                            -- We have values..
                                                            Just <|
                                                                -- So update the dict containing the missing grades
                                                                Dict.update mg.task_id
                                                                    (\secondEntry ->
                                                                        secondEntry
                                                                            |> Maybe.map (\es -> es ++ [ mg ])
                                                                            |> Maybe.withDefault [ mg ]
                                                                            |> Just
                                                                    )
                                                                    entry

                                                        Nothing ->
                                                            -- No previous key and thus no dict..
                                                            Just <|
                                                                -- Just create a dict containing the items
                                                                Dict.fromList [ ( mg.task_id, [ mg ] ) ]
                                                )
                                                d
                                        )
                                        Dict.empty
                        in
                        (CE.rRowExtraSpacing <|
                            [ span [ Styles.listHeadingStyle ] [ text "You have missing tasks to grade:" ]
                            ]
                        )
                            :: (sheettaskWithGrades
                                    |> Dict.toList
                                    |> List.map
                                        (\( sheetId, taskDict ) ->
                                            taskDict
                                                |> Dict.toList
                                                |> List.map
                                                    (\( taskId, mgs ) ->
                                                        CE.rRowHeaderActionButtons
                                                            ("On "
                                                                ++ (Dict.get sheetId model.sheetDict
                                                                        |> Maybe.map (\s -> s.name)
                                                                        |> Maybe.withDefault ""
                                                                   )
                                                                ++ " "
                                                                ++ (Dict.get taskId model.taskDict
                                                                        |> Maybe.map (\t -> t.name)
                                                                        |> Maybe.withDefault ""
                                                                   )
                                                                ++ " - "
                                                                ++ (String.fromInt <|
                                                                        List.length mgs
                                                                   )
                                                                ++ " Grades"
                                                            )
                                                            Styles.listHeadingStyle
                                                            [ ( "Grade"
                                                              , NavigateTo <|
                                                                    SubmissionGradingRoute
                                                                        fusedEnrollment.course.id
                                                                        taskId
                                                                        (Maybe.map
                                                                            (\gs ->
                                                                                gs
                                                                                    |> List.head
                                                                                    |> Maybe.map (\g -> g.id)
                                                                                    |> Maybe.withDefault 0
                                                                            )
                                                                            fusedEnrollment.groups
                                                                            |> Maybe.withDefault 0
                                                                        )
                                                              , Styles.buttonGreenStyle
                                                              )
                                                            ]
                                                    )
                                        )
                                    |> List.concat
                               )
               )


updateFusedEnrollmentWithTodosAndNextRequests : Model -> Int -> TodoRequest -> ( Model, Cmd Msg )
updateFusedEnrollmentWithTodosAndNextRequests model courseId todoRequest =
    (case todoRequest of
        TaskRequest response ->
            RemoteData.map (\mts -> TaskTodo mts) response

        GradeRequest response ->
            RemoteData.map (\mgs -> GradeTodo mgs) response
    )
        |> RemoteData.toMaybe
        |> (\maybeTodos ->
                ( { model
                    | fusedEnrollmentDict =
                        Dict.update courseId
                            (Maybe.map (\fe -> { fe | missingItems = maybeTodos }))
                            model.fusedEnrollmentDict
                  }
                , case maybeTodos of
                    Just todos ->
                        todos
                            |> (\todo ->
                                    case todo of
                                        TaskTodo tasks ->
                                            List.map
                                                (\t ->
                                                    ( t.course_id
                                                    , t.sheet_id
                                                    )
                                                )
                                                tasks

                                        GradeTodo grades ->
                                            List.map
                                                (\g ->
                                                    ( g.course_id
                                                    , g.sheet_id
                                                    )
                                                )
                                                grades
                               )
                            |> Set.fromList
                            |> Set.toList
                            |> List.map
                                (\( cid, sid ) ->
                                    Cmd.batch
                                        [ SheetRequests.sheetGet cid sid GetSheet
                                        , SheetRequests.sheetTasksGet cid sid GetTasks
                                        ]
                                )
                            |> Cmd.batch

                    Nothing ->
                        Cmd.none
                )
           )


updateFusedEnrollmentWithGroups : Model -> Int -> WebData (List Group) -> ( Model, Cmd Msg, SharedStateUpdate )
updateFusedEnrollmentWithGroups model courseId groupRequest =
    groupRequest
        |> RemoteData.toMaybe
        |> (\maybeGroup ->
                ( { model
                    | fusedEnrollmentDict =
                        Dict.update courseId
                            (Maybe.map (\fe -> { fe | groups = maybeGroup }))
                            model.fusedEnrollmentDict
                  }
                , Cmd.none
                , NoUpdate
                )
           )


updateSheetsDict : Model -> WebData Sheet -> Model
updateSheetsDict model sheetRequest =
    sheetRequest
        |> RemoteData.toMaybe
        |> Maybe.map
            (\sheet ->
                { model
                    | sheetDict =
                        Dict.insert sheet.id sheet model.sheetDict
                }
            )
        |> Maybe.withDefault model


updateTasksDict : Model -> WebData (List Task) -> Model
updateTasksDict model tasksRequest =
    tasksRequest
        |> RemoteData.toMaybe
        |> Maybe.map
            (\tasks ->
                List.foldl
                    (\task m ->
                        { m
                            | taskDict =
                                Dict.insert task.id task m.taskDict
                        }
                    )
                    model
                    tasks
            )
        |> Maybe.withDefault model


fillFusedEnrollmentAndDecideRequests : Model -> ( Model, Cmd Msg, SharedStateUpdate )
fillFusedEnrollmentAndDecideRequests model =
    let
        nextRequests =
            \m ->
                case ( m.getCourses, m.getAccountEnrollments ) of
                    ( Success _, Success _ ) ->
                        -- Only if both requests are successful
                        m.fusedEnrollmentDict
                            |> Dict.values
                            |> List.map
                                (\fe ->
                                    case fe.role of
                                        -- Decide what requests we need to send.
                                        -- In short for students the missing tasks and groups
                                        Student ->
                                            [ Cmd.map (GetMissingItem fe.course.id) <|
                                                CourseRequests.courseTaskMissing
                                                    fe.course.id
                                                    TaskRequest
                                            , CourseRequests.courseOwnGroupGet
                                                fe.course.id
                                                (GetOwnGroup fe.course.id)
                                            ]
                                                |> Cmd.batch

                                        -- and for tutors the missing grades and their groups
                                        Tutor ->
                                            [ Cmd.map
                                                (GetMissingItem fe.course.id)
                                              <|
                                                CourseRequests.courseGradeMissing
                                                    fe.course.id
                                                    GradeRequest
                                            , CourseRequests.courseOwnGroupGet
                                                fe.course.id
                                                (GetOwnGroup fe.course.id)
                                            ]
                                                |> Cmd.batch

                                        Admin ->
                                            Cmd.none
                                )
                            |> Cmd.batch

                    _ ->
                        Cmd.none
    in
    (case ( model.getCourses, model.getAccountEnrollments ) of
        ( Success courses, Success enrollments ) ->
            enrollments
                -- For every enrolled course...
                |> List.map
                    (\e ->
                        courses
                            -- Go through every overall course
                            |> List.filter (\c -> c.id == e.course_id)
                            |> List.head
                            -- And match the first course which matches the enrollment id
                            |> Maybe.map
                                (\c ->
                                    { course = c
                                    , role = e.role
                                    , missingItems = Nothing
                                    , groups = Nothing
                                    }
                                 -- Fill the fusedEnrollment record
                                )
                    )
                |> List.filterMap identity
                -- Remove all maybe values
                |> List.map (\fe -> ( fe.course.id, fe ))
                -- and prepare the key value pairs for the dict
                |> Dict.fromList
                |> (\fe -> { model | fusedEnrollmentDict = fe })

        -- Write the dict
        ( _, _ ) ->
            model
    )
        -- Setup the complete update
        |> (\m -> ( m, nextRequests m, NoUpdate ))
