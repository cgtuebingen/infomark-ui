{-
   This is the course detail page. It displays:
   - Basic information (Title, Description, Start and Ends dates etc.)
   - The course tutors as avatars with email, name etc
       - If you are a root user:
           - Option to search for all enrolled users and change the enrollment (tutor/student)

           (done)
   - Group information:
       - If you are a student and not distributed to a group:
           - The exercise groups with dates and tutors
           - Here you can set your preferences
       - If you are a student and distributed to a group:
           - Your group
       - If you are a tutor/supertutor:
           - Your group with date and times
           - A link to send emails (inline?)
       - If you are a root:
           - Options to create/edit/delete groups
           - Options to view all users in a group and an option to change a user from one group to another
   - Exercise sheets (download and link to the sheet for uploading/grading)
       - If you are a root:
           - Options to create/edit/delete sheets -> Extra view
   - Other course materials (Slides and Supplementary) (downloadable)
       - If you are a root:
           - Options to create/edit/delete materials (Inline?)
   - Statistics for Tutors and root
-}


module Pages.CourseDetail exposing (Model, Msg(..), init, update, view)

import Api.Data.AccountEnrollment as AccountEnrollment exposing (AccountEnrollment)
import Api.Data.Course exposing (Course)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.Exam exposing (Exam, ExamEnrollment, ExamEnrollments, Exams)
import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupBid as GroupBid exposing (GroupBid)
import Api.Data.GroupEnrollmentChange as GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Data.GroupSummary as GroupSummary exposing (GroupSummary)
import Api.Data.Material as Material exposing (Material, MaterialType(..))
import Api.Data.PointOverview as PointOverview exposing (PointOverview)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Api.Data.User as User exposing (User)
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Endpoint exposing (sheetFile, unwrap)
import Api.Request.Account as AccountRequests
import Api.Request.Courses as CoursesRequests
import Api.Request.Exam as ExamRequests
import Api.Request.Groups as GroupsRequests
import Api.Request.Material as MaterialRequests
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( checkBoxes
        , dateElement
        , dateInputElement
        , datesDisplayContainer
        , iconButton
        , inputElement
        , multiButton
        , nButtonList
        , normalPage
        , pageContainer
        , r2Column
        , rContainer
        , rRow
        , rRowExtraSpacing
        , rRowHeader
        , rRowHeaderActionButtons
        , searchElement
        , textAreaElement
        , timeInputElement
        , widePage
        )
import Components.Groups.AdminView as GroupAdminView
import Components.Groups.BiddingView as BiddingView
import Components.Groups.GroupView as GroupView
import Components.Toasty
import Components.UserAvatarEmailView as UserView
import Date
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug exposing (log, toString)
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import Markdown as MD
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import TimePicker exposing (TimeEvent(..), TimePicker)
import Toasty
import Utils.DateAndTimeUtils as DTU
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Utils.Utils exposing (flip, handleLogoutErrors, perform, tupleMapThree)


type Msg
    = NavigateTo Route
    | CourseResponse (WebData Course) -- Get basic information about the course
    | CourseRoleResponse (WebData (List AccountEnrollment)) -- Used to determine the course role
    | EnrollmentsResponse (WebData (List UserEnrollment)) -- List all enrollments in the course. Only used for students and tutors
    | SearchUserForEnrollmentResponse (WebData (List UserEnrollment)) -- Search for a specific user to change the enrollment
    | EnrollmentChangedResponse (WebData ()) -- Set the enrollment state for the searched user -- TODO set correct return
    | OwnGroupsResponse (WebData (List Group)) -- Show the assigned group for students. For tutors per default their group (can be changed). Not visible for admins
    | GroupsDisplayResponse (WebData (List Group)) -- Query all groups
    | GroupsSummaryResponse Int (WebData GroupSummary) -- Query all groups
    | SearchUserForGroupResponse (WebData UserEnrollment) -- Search for a specific user to change the group (Only admins)
    | GroupChangedResponse (WebData GroupEnrollmentChange) -- Response for a group change initiated by an admin
    | PointOverviewResponse (WebData (List PointOverview))
    | ToggleEnrollToExam Int
    | GroupMsg GroupMsgTypes
    | SheetRequestResponse (WebData (List Sheet))
    | MaterialRequestResponse (WebData (List Material))
    | SetField Field String
    | SearchUserForEnrollment
    | SearchUserForGroup
    | ChangeEnrollment UserEnrollment
    | ChangeGroup Int GroupEnrollmentChange
    | WriteTo Int
    | Download String
    | WriteEmailMsg Int
    | ExamEnrollmentResponse (WebData ExamEnrollments)
    | ExamsResponse (WebData Exams)
    | ToggleExamEnrollmentResponse Int (WebData ())
    | SetExamField Field String
    | ExamDateMsg Int DatePicker.Msg
    | ExamTimeMsg Int TimePicker.Msg
    | ExamUpdate Int
    | ExamCreateNew Exam
    | ExamCreateResponse (WebData ())
    | ExamUpdateResponse Int (WebData ())
    | ExamDelete Int
    | ExamDeleteResponse Int (WebData ())
    | ExamResponse
    | CreateExam


type alias Model =
    { courseId : Int
    , exams : Dict Int Exam
    , newExam : Exam
    , newExamVisible : Bool
    , examDates : Dict Int (Maybe Date.Date)
    , examTimes : Dict Int (Maybe TimePicker.Time)
    , examDatePickers : Dict Int DatePicker.DatePicker
    , examTimePickers : Dict Int TimePicker
    , examEnrollments : Dict Int ExamEnrollment
    , courseRole : Maybe CourseRole
    , courseRequest : WebData Course
    , sheetRequest : WebData (List Sheet)
    , materialRequest : WebData (List Material)
    , courseRoleRequest : WebData (List AccountEnrollment)
    , enrollmentsRequest : WebData (List UserEnrollment)
    , searchUserForEnrollmentRequest : WebData (List UserEnrollment)
    , enrollmentChangedRequest : WebData ()
    , ownGroupsRequest : WebData (List Group)
    , summaries : Dict Int GroupSummary
    , groupsRequest : WebData (List Group)
    , groupModel : Maybe GroupModel
    , searchUserForGroupRequest : WebData UserEnrollment
    , searchEnrollmentInput : String
    , searchGroupInput : String
    , pointOverviewResponse : WebData (List PointOverview)
    }


type GroupMsgTypes
    = BiddingMsg BiddingView.Msg
    | DetailMsg GroupView.Msg
    | GroupAdminMsg GroupAdminView.Msg


type GroupModel
    = BiddingModel BiddingView.Model
    | DetailModel GroupView.Model
    | GroupAdminModel GroupAdminView.Model


init : Int -> ( Model, Cmd Msg )
init id =
    ( { courseId = id
      , exams = Dict.empty
      , examDates = Dict.empty
      , examTimes = Dict.empty
      , examDatePickers = Dict.empty
      , examTimePickers = Dict.empty
      , examEnrollments = Dict.empty
      , newExamVisible = False
      , newExam = initNewExam
      , courseRole = Nothing
      , courseRequest = Loading
      , sheetRequest = Loading
      , materialRequest = Loading
      , courseRoleRequest = Loading
      , enrollmentsRequest = NotAsked
      , searchUserForEnrollmentRequest = NotAsked
      , enrollmentChangedRequest = NotAsked
      , ownGroupsRequest = NotAsked
      , groupsRequest = NotAsked
      , summaries = Dict.empty
      , groupModel = Nothing
      , searchUserForGroupRequest = NotAsked
      , searchEnrollmentInput = ""
      , searchGroupInput = ""
      , pointOverviewResponse = Loading
      }
    , Cmd.batch
        [ AccountRequests.accountEnrollmentGet CourseRoleResponse
        , CoursesRequests.courseGet id CourseResponse
        , CoursesRequests.courseSheetsGet id SheetRequestResponse
        , CoursesRequests.courseMaterialsGet id MaterialRequestResponse
        , CoursesRequests.coursePointsGet id PointOverviewResponse
        , ExamRequests.examsGet id ExamsResponse
        , ExamRequests.examEnrollmentsStudentGet ExamEnrollmentResponse
        ]
    )


initNewExam : Exam
initNewExam =
    Exam 42 "Neue Prüfung" "Beschreibung" (Time.millisToPosix 0) 0


determineInitialRoleRequests : Model -> CourseRole -> ( Model, Cmd Msg )
determineInitialRoleRequests model role =
    case role of
        Admin ->
            ( { model
                | groupsRequest = Loading
              }
            , CoursesRequests.courseGroupsGet model.courseId GroupsDisplayResponse
            )

        Tutor ->
            ( { model
                | enrollmentsRequest = Loading
                , ownGroupsRequest = Loading
              }
            , Cmd.batch
                [ CoursesRequests.coursesEnrollmentGetTeam model.courseId EnrollmentsResponse
                , CoursesRequests.courseOwnGroupGet model.courseId OwnGroupsResponse
                , CoursesRequests.courseGroupsGet model.courseId GroupsDisplayResponse
                ]
            )

        Student ->
            ( { model
                | enrollmentsRequest = Loading
                , ownGroupsRequest = Loading
              }
            , Cmd.batch
                [ CoursesRequests.coursesEnrollmentGetTeam model.courseId EnrollmentsResponse
                , CoursesRequests.courseOwnGroupGet model.courseId OwnGroupsResponse
                ]
            )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        CourseResponse response ->
            ( { model | courseRequest = response }, Cmd.none, NoUpdate )

        CourseRoleResponse (Success roles) ->
            case determineRole model.courseId roles of
                Just role ->
                    -- We received the users course role.
                    let
                        -- Determine what commands need to be send depending on the role
                        updatedModel =
                            { model | courseRole = Just role, courseRoleRequest = Success roles }

                        ( newModel, newCmds ) =
                            determineInitialRoleRequests updatedModel role
                    in
                    ( newModel, newCmds, NoUpdate )

                -- Execute the changes
                Nothing ->
                    -- Whoops. The user is not enrolled in the course. Navigate back to the courses page
                    ( model, pushUrl sharedState.navKey (reverseRoute CoursesRoute), NoUpdate )

        SearchUserForEnrollment ->
            ( { model | searchUserForEnrollmentRequest = Loading }, CoursesRequests.coursesEnrollmentGetByEmail model.courseId model.searchEnrollmentInput SearchUserForEnrollmentResponse, NoUpdate )

        SearchUserForEnrollmentResponse response ->
            ( { model | searchUserForEnrollmentRequest = response }, Cmd.none, NoUpdate )

        EnrollmentsResponse response ->
            ( { model | enrollmentsRequest = response }, Cmd.none, NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        ChangeEnrollment enrollment ->
            ( model
            , CoursesRequests.coursesEnrollmentsUserPut model.courseId
                enrollment.user.id
                enrollment.role
                EnrollmentChangedResponse
            , NoUpdate
            )

        EnrollmentChangedResponse (Success _) ->
            ( model, perform SearchUserForEnrollment, NoUpdate )

        SheetRequestResponse response ->
            ( { model | sheetRequest = response }, Cmd.none, NoUpdate )

        MaterialRequestResponse response ->
            ( { model | materialRequest = response }, Cmd.none, NoUpdate )

        Download url ->
            ( model, Download.url url, NoUpdate )

        WriteTo userId ->
            ( model, UserView.updateFromUserAvatar sharedState userId, NoUpdate )

        WriteEmailMsg _ ->
            ( model, Cmd.none, NoUpdate )

        OwnGroupsResponse response ->
            updateGroupDisplay sharedState { model | ownGroupsRequest = response }

        GroupsDisplayResponse response ->
            updateGroupDisplay sharedState { model | groupsRequest = response }

        GroupsSummaryResponse groupId response ->
            case response of
                Success summary ->
                    let
                        newSummaries =
                            Dict.insert groupId summary model.summaries

                        allGroups =
                            case model.groupsRequest of
                                Success groups ->
                                    groups

                                _ ->
                                    []

                        ownGroups =
                            case model.ownGroupsRequest of
                                Success groups ->
                                    groups

                                _ ->
                                    []

                        groupInit =
                            Tuple.mapBoth
                                DetailModel
                                (Cmd.map DetailMsg)
                            <|
                                GroupView.init model.courseId
                                    ownGroups
                                    allGroups
                                    Tutor
                                    newSummaries
                    in
                    ( { model
                        | groupModel = Just <| Tuple.first groupInit
                        , summaries = newSummaries
                      }
                    , Cmd.none
                    , NoUpdate
                    )

                Failure err ->
                    let
                        errorMsg =
                            case err of
                                Http.BadUrl url ->
                                    "BadUrl: " ++ url

                                Http.Timeout ->
                                    "Timeout"

                                Http.NetworkError ->
                                    "Networerror"

                                Http.BadStatus status ->
                                    "BadStatus: " ++ String.fromInt status

                                Http.BadBody dbg ->
                                    "BadBody: " ++ dbg
                    in
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" errorMsg
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" "Summary response failed somehow"
                    )

        GroupMsg subMsg ->
            let
                ( subModel, subCmd, subSharedState ) =
                    case ( model.groupModel, subMsg ) of
                        ( Just (BiddingModel biddingModel), BiddingMsg bidMsg ) ->
                            tupleMapThree
                                (\m -> Just <| BiddingModel m)
                                (\m -> Cmd.map GroupMsg <| Cmd.map BiddingMsg m)
                                identity
                            <|
                                BiddingView.update sharedState bidMsg biddingModel

                        ( Just (DetailModel detailModel), DetailMsg detailMsg ) ->
                            tupleMapThree
                                (\m -> Just <| DetailModel m)
                                (\m -> Cmd.map GroupMsg <| Cmd.map DetailMsg m)
                                identity
                            <|
                                GroupView.update sharedState detailMsg detailModel

                        ( Just (GroupAdminModel adminModel), GroupAdminMsg adminMsg ) ->
                            tupleMapThree
                                (\m -> Just <| GroupAdminModel m)
                                (\m -> Cmd.map GroupMsg <| Cmd.map GroupAdminMsg m)
                                identity
                            <|
                                GroupAdminView.update sharedState adminMsg adminModel

                        ( _, _ ) ->
                            ( Nothing, Cmd.none, NoUpdate )
            in
            ( { model | groupModel = subModel }
            , subCmd
            , subSharedState
            )

        PointOverviewResponse response ->
            ( { model | pointOverviewResponse = response }
            , Cmd.none
            , NoUpdate
            )

        ExamsResponse response ->
            case response of
                Success exams ->
                    let
                        newExams =
                            Dict.fromList
                                (List.map
                                    (\e ->
                                        ( e.id, e )
                                    )
                                    exams
                                )

                        datePickers =
                            Dict.map
                                (\id e ->
                                    DatePicker.initFromDate <|
                                        Date.fromPosix
                                            (Maybe.withDefault
                                                Time.utc
                                                sharedState.timezone
                                            )
                                            (Maybe.withDefault
                                                (Time.millisToPosix 0)
                                                (Just e.exam_time)
                                            )
                                )
                                newExams

                        examDates =
                            Dict.map
                                (\id e ->
                                    Just
                                        (Date.fromPosix
                                            (Maybe.withDefault Time.utc
                                                sharedState.timezone
                                            )
                                            (Maybe.withDefault
                                                (Time.millisToPosix 0)
                                                (Just e.exam_time)
                                            )
                                        )
                                )
                                newExams

                        examTimes =
                            Dict.map
                                (\id e ->
                                    Just
                                        (DTU.pickerTimeFromPosix
                                            (Maybe.withDefault Time.utc sharedState.timezone)
                                            e.exam_time
                                        )
                                )
                                newExams

                        timePickers =
                            Dict.map
                                (\id time ->
                                    TimePicker.init <| time
                                )
                                examTimes
                    in
                    ( { model
                        | exams = newExams
                        , examDates = examDates
                        , examDatePickers = datePickers
                        , examTimes = examTimes
                        , examTimePickers = timePickers
                      }
                    , Cmd.none
                    , NoUpdate
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ExamEnrollmentResponse response ->
            case response of
                Success enrollments ->
                    let
                        examEnrollments =
                            Dict.fromList
                                (List.map
                                    (\en ->
                                        ( en.exam_id, en )
                                    )
                                    enrollments
                                )
                    in
                    ( { model | examEnrollments = examEnrollments }
                    , Cmd.none
                    , NoUpdate
                    )

                Failure err ->
                    let
                        errorMsg =
                            case err of
                                Http.BadUrl url ->
                                    "BadUrl: " ++ url

                                Http.Timeout ->
                                    "Timeout"

                                Http.NetworkError ->
                                    "Networerror"

                                Http.BadStatus status ->
                                    "BadStatus: " ++ String.fromInt status

                                Http.BadBody dbg ->
                                    "BadBody: " ++ dbg
                    in
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" errorMsg
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ToggleEnrollToExam exam_id ->
            ( model
            , if Dict.member exam_id model.examEnrollments then
                ExamRequests.examEnrollmentDelete model.courseId
                    exam_id
                    (ToggleExamEnrollmentResponse
                        exam_id
                    )

              else
                ExamRequests.examEnrollmentPost model.courseId
                    exam_id
                    (ToggleExamEnrollmentResponse
                        exam_id
                    )
            , NoUpdate
            )

        ToggleExamEnrollmentResponse exam_id response ->
            case response of
                Success _ ->
                    ( model
                    , ExamRequests.examEnrollmentsStudentGet ExamEnrollmentResponse
                    , ShowToast <|
                        Components.Toasty.Success "Success"
                            "Änderung gespeichert"
                    )

                Failure err ->
                    let
                        errorMsg =
                            case err of
                                Http.BadUrl url ->
                                    "BadUrl: " ++ url

                                Http.Timeout ->
                                    "Timeout"

                                Http.NetworkError ->
                                    "Networerror"

                                Http.BadStatus status ->
                                    "BadStatus: " ++ String.fromInt status

                                Http.BadBody dbg ->
                                    "BadBody: " ++ dbg
                    in
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" errorMsg
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" "Could not save changes!"
                    )

        -- Update enrollments in model!
        SetExamField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        ExamDateMsg examId subMsg ->
            let
                maybeOldPicker =
                    Dict.get examId model.examDatePickers

                maybeOldDate =
                    Dict.get examId model.examDates
            in
            case ( maybeOldPicker, maybeOldDate ) of
                ( Just oldPicker, Just oldDate ) ->
                    let
                        ( newDatePicker, event ) =
                            DatePicker.update
                                (datePickerSettings sharedState)
                                subMsg
                                oldPicker

                        newDate =
                            case event of
                                Picked date ->
                                    Just date

                                _ ->
                                    oldDate
                    in
                    ( { model
                        | examDatePickers =
                            Dict.insert examId
                                newDatePicker
                                model.examDatePickers
                        , examDates = Dict.insert examId newDate model.examDates
                      }
                    , Cmd.none
                    , NoUpdate
                    )

                ( _, _ ) ->
                    ( model
                    , Cmd.none
                    , NoUpdate
                    )

        ExamTimeMsg examId subMsg ->
            let
                maybeOldPicker =
                    Dict.get examId model.examTimePickers

                maybeOldTime =
                    Dict.get examId model.examTimes
            in
            case ( maybeOldPicker, maybeOldTime ) of
                ( Just oldPicker, Just oldTime ) ->
                    let
                        ( newTimePicker, event ) =
                            TimePicker.update timePickerSettings subMsg oldPicker

                        newTime =
                            case event of
                                Changed time ->
                                    time

                                NoChange ->
                                    oldTime
                    in
                    ( { model
                        | examTimePickers =
                            Dict.insert examId
                                newTimePicker
                                model.examTimePickers
                        , examTimes = Dict.insert examId newTime model.examTimes
                      }
                    , Cmd.none
                    , NoUpdate
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, NoUpdate )

        ExamCreateNew exam ->
            let
                maybeDate =
                    Dict.get exam.id model.examDates

                maybeTime =
                    Dict.get exam.id model.examTimes
            in
            case ( maybeDate, maybeTime ) of
                ( Just (Just date), Just (Just time) ) ->
                    let
                        newExamTime =
                            DTU.joinDateTimeAndOffset date
                                time
                                -- TODO this needs to be fixed!!
                                { multiplier = 1, hours = 2, minutes = 0 }

                        newExam =
                            Exam exam.id
                                exam.name
                                exam.description
                                newExamTime
                                exam.course_id
                    in
                    ( model
                    , ExamRequests.examPost model.courseId newExam ExamCreateResponse
                    , NoUpdate
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, NoUpdate )

        ExamCreateResponse response ->
            case response of
                Success _ ->
                    ( { model
                        | exams = Dict.empty
                        , examDates = Dict.empty
                        , examDatePickers = Dict.empty
                        , examTimes = Dict.empty
                        , examTimePickers = Dict.empty
                      }
                    , ExamRequests.examsGet model.courseId ExamsResponse
                    , NoUpdate
                    )

                Failure _ ->
                    ( model
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Error "Error"
                            "Failed to save new exam"
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ExamDelete examId ->
            ( model
            , ExamRequests.examDelete model.courseId
                examId
                (ExamDeleteResponse
                    examId
                )
            , NoUpdate
            )

        ExamDeleteResponse examId response ->
            case response of
                Success _ ->
                    ( { model
                        | exams = Dict.remove examId model.exams
                        , examDates = Dict.remove examId model.examDates
                        , examDatePickers = Dict.remove examId model.examDatePickers
                        , examTimes = Dict.remove examId model.examTimes
                        , examTimePickers = Dict.remove examId model.examTimePickers
                      }
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Success "Success"
                            "Prüfung wurde gelöscht."
                    )

                Failure _ ->
                    ( model
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Error "Error"
                            "Could not delete Exam"
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ExamUpdate examId ->
            let
                maybeOldExam =
                    Dict.get examId model.exams

                maybeDate =
                    Dict.get examId model.examDates

                maybeTime =
                    Dict.get examId model.examTimes
            in
            case ( maybeOldExam, maybeDate, maybeTime ) of
                ( Just oldExam, Just (Just date), Just (Just time) ) ->
                    let
                        newExamTime =
                            DTU.joinDateTimeAndOffset date
                                time
                                -- TODO this needs to be fixed!!
                                { multiplier = 1, hours = 2, minutes = 0 }

                        newExam =
                            Exam examId
                                oldExam.name
                                oldExam.description
                                newExamTime
                                oldExam.course_id
                    in
                    ( model
                    , ExamRequests.examPut model.courseId examId newExam (ExamUpdateResponse examId)
                    , NoUpdate
                    )

                ( _, _, _ ) ->
                    ( model
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Error "Error" "Updating exam failed"
                    )

        ExamUpdateResponse examId response ->
            case response of
                Success _ ->
                    let
                        maybeOldExam =
                            Dict.get examId model.exams

                        maybeDate =
                            Dict.get examId model.examDates

                        maybeTime =
                            Dict.get examId model.examTimes
                    in
                    case ( maybeOldExam, maybeDate, maybeTime ) of
                        ( Just oldExam, Just (Just date), Just (Just time) ) ->
                            let
                                newExamTime =
                                    DTU.joinDateTimeAndOffset date
                                        time
                                        { multiplier = 1, hours = 0, minutes = 0 }

                                newExam =
                                    Exam examId
                                        oldExam.name
                                        oldExam.description
                                        newExamTime
                                        oldExam.course_id
                            in
                            ( { model
                                | exams =
                                    Dict.insert examId newExam model.exams
                              }
                            , Cmd.none
                            , ShowToast <|
                                Components.Toasty.Success "Success" "Änderung gespeichert."
                            )

                        ( _, _, _ ) ->
                            ( model
                            , Cmd.none
                            , ShowToast <|
                                Components.Toasty.Success "Success" "Änderungn gespeichert. Darstellungsfehler."
                            )

                Failure _ ->
                    ( model
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Error "Error" "Server reports: Updating exam failed"
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , NoUpdate
                    )

        CreateExam ->
            let
                newExam =
                    initNewExam

                curTime =
                    Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime

                exams =
                    Dict.insert newExam.id
                        { newExam
                            | exam_time = curTime
                            , course_id = model.courseId
                        }
                        model.exams

                examDates =
                    Dict.insert newExam.id
                        (Just
                            (Date.fromPosix
                                (Maybe.withDefault Time.utc
                                    sharedState.timezone
                                )
                                (Maybe.withDefault
                                    (Time.millisToPosix 0)
                                    (Just curTime)
                                )
                            )
                        )
                        model.examDates

                datePickers =
                    Dict.insert newExam.id
                        (DatePicker.initFromDate
                            (Date.fromPosix
                                (Maybe.withDefault
                                    Time.utc
                                    sharedState.timezone
                                )
                                (Maybe.withDefault
                                    (Time.millisToPosix 0)
                                    (Just curTime)
                                )
                            )
                        )
                        model.examDatePickers

                examTimes =
                    Dict.insert newExam.id
                        (Just
                            (DTU.pickerTimeFromPosix
                                (Maybe.withDefault Time.utc sharedState.timezone)
                                curTime
                            )
                        )
                        model.examTimes

                timePickers =
                    Dict.insert newExam.id
                        (TimePicker.init
                            (Just
                                (DTU.pickerTimeFromPosix
                                    (Maybe.withDefault Time.utc sharedState.timezone)
                                    curTime
                                )
                            )
                        )
                        model.examTimePickers
            in
            ( { model
                | exams = exams
                , examDates = examDates
                , examDatePickers = datePickers
                , examTimes = examTimes
                , examTimePickers = timePickers
              }
            , Cmd.none
            , NoUpdate
            )

        _ ->
            ( model, Cmd.none, NoUpdate )


updateGroupDisplay : SharedState -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
updateGroupDisplay sharedState model =
    case ( model.ownGroupsRequest, model.courseRole ) of
        ( Failure err, Just Student ) ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        maybeInit =
                            case e of
                                Http.BadStatus 404 ->
                                    Just <|
                                        Tuple.mapBoth
                                            (\subModel -> BiddingModel subModel)
                                            (\subMsg -> Cmd.map BiddingMsg subMsg)
                                        <|
                                            BiddingView.init model.courseId

                                _ ->
                                    Nothing
                    in
                    ( { model
                        | groupModel = Maybe.map Tuple.first maybeInit
                      }
                    , Maybe.withDefault Cmd.none <|
                        Maybe.map
                            (\subInit ->
                                Cmd.map GroupMsg <| Tuple.second subInit
                            )
                            maybeInit
                    , NoUpdate
                    )
                )
                err

        ( Success ownGroups, _ ) ->
            case ( model.groupsRequest, model.courseRole ) of
                ( _, Just Student ) ->
                    let
                        groupInit =
                            Tuple.mapBoth
                                DetailModel
                                (Cmd.map DetailMsg)
                            <|
                                GroupView.init model.courseId
                                    ownGroups
                                    []
                                    Student
                                    model.summaries
                    in
                    ( { model
                        | groupModel = Just <| Tuple.first groupInit
                      }
                    , Cmd.map GroupMsg <| Tuple.second groupInit
                    , NoUpdate
                    )

                ( Success allGroups, Just Tutor ) ->
                    let
                        groupInit =
                            Tuple.mapBoth
                                DetailModel
                                (Cmd.map DetailMsg)
                            <|
                                GroupView.init model.courseId
                                    ownGroups
                                    allGroups
                                    Tutor
                                    model.summaries
                    in
                    ( { model
                        | groupModel = Just <| Tuple.first groupInit
                      }
                    , Cmd.batch
                        ([ Cmd.map GroupMsg <| Tuple.second groupInit ]
                            ++ List.map
                                -- Request group overview for all groups
                                (\g ->
                                    CoursesRequests.courseGroupSummaryPerGroup
                                        model.courseId
                                        g.id
                                        (GroupsSummaryResponse g.id)
                                )
                                ownGroups
                        )
                    , NoUpdate
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, NoUpdate )

        ( _, Just Admin ) ->
            case model.groupsRequest of
                Success groups ->
                    let
                        groupInit =
                            Tuple.mapBoth
                                GroupAdminModel
                                (Cmd.map GroupAdminMsg)
                            <|
                                GroupAdminView.init model.courseId groups
                    in
                    ( { model
                        | groupModel = Just <| Tuple.first groupInit
                      }
                    , Cmd.map GroupMsg <| Tuple.second groupInit
                    , NoUpdate
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ( _, _ ) ->
            ( model, Cmd.none, NoUpdate )



-- TODO if successful update searchProgress too


view : SharedState -> Model -> Html Msg
view sharedState model =
    case ( model.courseRoleRequest, model.courseRole ) of
        ( Success _, Just role ) ->
            pageContainer <|
                [ widePage <| [ viewCourseInfo sharedState model ]
                , viewExams sharedState model
                , normalPage <|
                    [ viewSheets sharedState model
                    , viewMaterials sharedState model Slide
                    , viewMaterials sharedState model Supplementary
                    , viewDetermineGroupDisplay role sharedState model
                    , viewDetermineTeamOrSearch role sharedState model
                    ]
                ]

        ( _, _ ) ->
            div [] []


viewExams : SharedState -> Model -> Html Msg
viewExams sharedState model =
    case model.courseRole of
        Just Admin ->
            viewExamsAdmin sharedState model

        Just Tutor ->
            text ""

        Just Student ->
            viewExamsStudents sharedState model

        Nothing ->
            text ""


viewExamsAdmin : SharedState -> Model -> Html Msg
viewExamsAdmin sharedState model =
    normalPage <|
        [ rRowHeader "Klausuren"
        , rRow
            (Dict.toList model.exams
                |> List.map
                    (\( id, exam ) ->
                        viewExamEditor
                            sharedState
                            model
                            exam
                    )
            )
        , nButtonList
            [ { button1_icon = "add_circle"
              , button1_msg = CreateExam
              , label = "Create New Exam"
              , right_buttons = []
              }
            ]
        ]


viewExamEditor : SharedState -> Model -> Exam -> Html Msg
viewExamEditor sharedState model exam =
    let
        maybeDatePicker =
            Dict.get exam.id model.examDatePickers

        maybeExamDate =
            Dict.get exam.id model.examDates

        maybeExamTime =
            Dict.get exam.id model.examTimes

        maybeTimePicker =
            Dict.get exam.id model.examTimePickers
    in
    case ( ( maybeDatePicker, maybeExamDate ), ( maybeExamTime, maybeTimePicker ) ) of
        ( ( Just datePicker, Just examDate ), ( Just examTime, Just timePicker ) ) ->
            rContainer
                [ rRow
                    (inputElement
                        { label = "Bezeichnung"
                        , placeholder = "Hauptklausur"
                        , fieldType = "text"
                        , value = exam.name
                        }
                        (ExamName exam.id)
                        []
                        SetExamField
                    )
                , rRow
                    (textAreaElement
                        { label = "Bezeichnung"
                        , placeholder = "Hörsaal N7, N8, 90 Minuten"
                        , value = exam.description
                        , rows = 4
                        }
                        (ExamDescription exam.id)
                        []
                        SetExamField
                    )
                , rRow <|
                    r2Column
                        (dateInputElement
                            { label = "Datum"
                            , datePicker = datePicker
                            , settings = datePickerSettings sharedState
                            , value = examDate
                            }
                            ExamDate
                            []
                            (ExamDateMsg exam.id)
                        )
                        (timeInputElement
                            { label = "Uhrzeit"
                            , placeholder = "Uhrzeit auswählen"
                            , timePicker = timePicker
                            , settings = timePickerSettings
                            }
                            ExamTime
                            []
                            (ExamTimeMsg exam.id)
                        )
                , if exam.id == 42 then
                    rRow
                        [ iconButton "cloud_upload" (ExamCreateNew exam) ]

                  else
                    rRow
                        [ iconButton "check" (ExamUpdate exam.id)
                        , iconButton "delete" (ExamDelete exam.id)
                        ]
                ]

        ( ( _, _ ), ( _, _ ) ) ->
            text "ERROR on date/time pickers"


datePickerSettings : SharedState -> DatePicker.Settings
datePickerSettings sharedState =
    let
        curTime =
            Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime
    in
    { defaultSettings
        | inputAttributes =
            [ Styles.lineInputStyle
            , classes [ TC.w_100, TC.mb3 ]
            ]

        -- , dateFormatter = DF.dateToShortFormatString sharedState
        , dayFormatter = DF.shortDayFormatter sharedState
        , monthFormatter = DF.monthFormatter sharedState
    }


timePickerSettings : TimePicker.Settings
timePickerSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
    { defaultSettings | showSeconds = False, minuteStep = 15, use24Hours = True }


viewExamsStudents : SharedState -> Model -> Html Msg
viewExamsStudents sharedState model =
    if Dict.isEmpty model.exams then
        text ""

    else if Dict.isEmpty model.examEnrollments then
        div [ classes [ TC.w_100, "bg-light-gold", TC.pa4 ] ]
            [ normalPage <|
                [ rRowHeader "Klausur"
                , viewExamEnrollmentRequest sharedState model
                , viewExamEnrollmentForm sharedState model
                ]
            ]

    else
        div [ classes [ TC.w_100, "bg-light-gold", TC.pa4 ] ]
            [ normalPage <|
                [ rRowHeader "Klausur"
                , viewExamEnrollmentForm sharedState model
                ]
            ]


viewExamEnrollmentRequest : SharedState -> Model -> Html Msg
viewExamEnrollmentRequest sharedState model =
    div [ classes [ "bg-dark-red", TC.pa2, TC.white_90 ] ]
        [ rRow
            [ text "Sie sind bisher "
            , span [ classes [ TC.b ] ] [ text "NICHT " ]
            , text "zur Klausur angemeldet! "
            , text "Sie dürfen nur mitschreiben, wenn Sie sich hier "
            , text "anmelden!"
            ]
        , rRow
            [ text "Wenn Sie sich zur Haptklausur anmelden und durchfallen, dürfen Sie automatisch die Nachklausur mitschreiben." ]
        , rRow
            [ text "Bitte melden Sie sich explizit nur zur Nachklausur an,"
            , text " wenn Sie aus wichtigen Gründen die Hauptklausur nicht mitschreiben können."
            ]
        ]


isExamOver : SharedState -> Exam -> Bool
isExamOver sharedState exam =
    Time.posixToMillis exam.exam_time
        < (Maybe.withDefault 0 <|
            Maybe.map Time.posixToMillis
                sharedState.currentTime
          )


viewExamDetails : SharedState -> Exam -> Html Msg
viewExamDetails sharedState exam =
    rRow
        [ span [ classes [ TC.b ] ]
            [ text exam.name
            ]
        , text " am "
        , span
            [ classes [ TC.b ] ]
            [ text (DF.shortDateFormatter sharedState exam.exam_time)
            ]
        ]


viewExamEnrollmentForm : SharedState -> Model -> Html Msg
viewExamEnrollmentForm sharedState model =
    rRow
        (List.map
            (\( id, e ) ->
                if isExamOver sharedState e then
                    case Dict.get id model.examEnrollments of
                        Just enrollment ->
                            case enrollment.status of
                                0 ->
                                    div []
                                        [ viewExamDetails sharedState e
                                        , rRow <|
                                            [ text "Sie haben nicht an dieser Klausur teilgenommen."
                                            ]
                                        ]

                                1 ->
                                    div []
                                        [ viewExamDetails sharedState e
                                        , rRow <|
                                            [ text "Sie haben mit der Note '"
                                            , span
                                                [ classes [ TC.b ] ]
                                                [ text enrollment.mark
                                                ]
                                            , text "' bestanden. Eventuelle Boni sind bereits mit eingerechnet."
                                            ]
                                        , rRow <|
                                            [ span [ classes [ TC.f7 ] ]
                                                [ text "Die Angabe zur Note ist ohne Gewähr und nur zu Ihrer Information. Es gilt die ans Prüfungssekretariat gemeldete Note."
                                                ]
                                            ]
                                        ]

                                2 ->
                                    div []
                                        [ viewExamDetails sharedState e
                                        , rRow <|
                                            [ text "Sie sind mit der Note '"
                                            , span
                                                [ classes [ TC.b ] ]
                                                [ text enrollment.mark
                                                ]
                                            , text "' durchgefallen."
                                            , rRow <|
                                                [ span [ classes [ TC.f7 ] ]
                                                    [ text "Die Angabe zur Note ist ohne Gewähr und nur zu Ihrer Information. Es gilt die ans Prüfungssekretariat gemeldete Note."
                                                    ]
                                                ]
                                            ]
                                        ]

                                _ ->
                                    div []
                                        [ viewExamDetails sharedState e
                                        , text "Undefinierter Zustand."
                                        ]

                        Nothing ->
                            text "Gelöscht aus gründen"

                else
                    let
                        examInfo =
                            { label =
                                e.name
                                    ++ " "
                                    ++ DF.shortDateFormatter sharedState e.exam_time
                                    ++ " "
                                    ++ DF.shortTimeFormatString sharedState e.exam_time
                            , description = e.description
                            , isChecked = Dict.member id model.examEnrollments
                            , message = ToggleEnrollToExam id
                            }

                        enrolledInfo =
                            if Dict.member id model.examEnrollments then
                                span [ classes [ TC.dark_green ] ]
                                    [ text "Sie sind zu der Klausur angemeldet. "
                                    , text "Entfernen Sie das Häkchen, wenn Sie nicht mitschreiben möchten"
                                    ]

                            else
                                span [ classes [ TC.dark_red ] ]
                                    [ text "Sie sind NICHT zu der Klausur angemeldet. "
                                    , text "Setzten Sie das Häkchen, wenn Sie mitschreiben möchten."
                                    ]
                    in
                    rRow
                        [ div [ classes [] ] <|
                            checkBoxes [ examInfo ]
                                ++ [ enrolledInfo ]
                        ]
            )
            (filterExamsForSuccess model.exams model.examEnrollments)
        )


filterExamsForSuccess : Dict Int Exam -> Dict Int ExamEnrollment -> List ( Int, Exam )
filterExamsForSuccess exams enrollments =
    let
        containsSuccess =
            Dict.foldl successFoldFunction False enrollments
    in
    if containsSuccess then
        filterExam exams enrollments

    else
        Dict.toList exams


successFoldFunction : Int -> ExamEnrollment -> Bool -> Bool
successFoldFunction key exam acc =
    acc || (exam.status == 1)


filterExam : Dict Int Exam -> Dict Int ExamEnrollment -> List ( Int, Exam )
filterExam exams enrollments =
    let
        joinedExamAndEnrollment =
            Dict.merge
                (\_ _ acc -> acc)
                (\key a b -> Dict.insert key ( a, b ))
                (\_ _ acc -> acc)
                exams
                enrollments
                Dict.empty

        examSuccess =
            List.map
                (\( id, ( exam, enrollment ) ) ->
                    if enrollment.status == 1 then
                        ( True, id, exam )

                    else
                        ( False, id, exam )
                )
                (Dict.toList joinedExamAndEnrollment)
    in
    List.map (\( use, id, exam ) -> ( id, exam ))
        (List.filter (\( use, id, exam ) -> use) examSuccess)


viewCourseInfo : SharedState -> Model -> Html Msg
viewCourseInfo sharedState model =
    let
        maybePoints =
            case ( model.pointOverviewResponse, model.courseRequest ) of
                ( Success points, Success course ) ->
                    if List.isEmpty points then
                        Nothing

                    else
                        points
                            |> List.map (\p -> ( p.acquired_points, p.max_points ))
                            |> List.foldl
                                (\pt at ->
                                    Tuple.mapBoth
                                        ((+) <| Tuple.first pt)
                                        ((+) <| Tuple.second pt)
                                        at
                                )
                                ( 0, 0 )
                            |> (\pt ->
                                    ( Tuple.first pt
                                    , Tuple.second pt
                                    , let
                                        acquiredPerc =
                                            round <|
                                                (toFloat <| Tuple.first pt)
                                                    / toFloat 302
                                                    -- (toFloat <| Tuple.second pt)
                                                    * 100
                                      in
                                      if acquiredPerc < course.required_percentage then
                                        TC.red

                                      else if acquiredPerc < (course.required_percentage + 5) then
                                        TC.gold

                                      else
                                        TC.dark_green
                                    )
                               )
                            |> Just

                ( _, _ ) ->
                    Nothing
    in
    case model.courseRequest of
        RemoteData.Success course ->
            div [ classes [ TC.w_100, TC.b__gray, "small-border", TC.bb, TC.pb3, TC.overflow_hidden ] ]
                (r2Column
                    [ div []
                        [ h1 [ classes [ TC.mb3, TC.mt0, TC.lh_title ] ] [ text course.name ]
                        , datesDisplayContainer <|
                            (dateElement "Beginn " <| DF.fullDateFormatter sharedState course.begins_at)
                                ++ (dateElement "Ende " <| DF.fullDateFormatter sharedState course.ends_at)
                                ++ (case maybePoints of
                                        Just ( acquired, max, color ) ->
                                            [ dt [ classes [ TC.black, TC.fw6 ] ] [ text "Erreichte Punkte:" ]
                                            , h1 [ classes [ color, TC.mt0 ], Styles.headerStyle ]
                                                [ text <|
                                                    (String.fromInt <| acquired)
                                                        ++ "/"
                                                        ++ "302"

                                                -- (String.fromInt <| max)
                                                ]
                                            ]

                                        Nothing ->
                                            [ text "" ]
                                   )
                        ]
                    ]
                    [ MD.toHtml [ Styles.textStyle ] <| course.description ]
                )

        _ ->
            text ""



-- TODO loading, error etc.


viewDetermineTeamOrSearch : CourseRole -> SharedState -> Model -> Html Msg
viewDetermineTeamOrSearch courseRole sharedState model =
    case courseRole of
        Admin ->
            viewMemberSearch sharedState model

        _ ->
            viewTeam sharedState model


viewTeam : SharedState -> Model -> Html Msg
viewTeam sharedState model =
    case model.enrollmentsRequest of
        RemoteData.Success enrollments ->
            let
                sortedTeam =
                    List.sortWith compareRoleName enrollments
            in
            rContainer <|
                [ rRowHeader "Team"
                , div [ classes [ TC.flex, TC.flex_row, TC.flex_wrap, TC.justify_start ] ]
                    (sortedTeam
                        |> List.map (\ue -> UserView.initFromUser ue.user)
                        |> List.map Tuple.first
                        |> List.map
                            (\uv ->
                                div
                                    [ classes
                                        [ TC.w_third_l
                                        , TC.w_50_m
                                        , TC.w_100
                                        ]
                                    ]
                                    [ UserView.view sharedState uv (Just WriteTo) ]
                            )
                    )
                ]

        _ ->
            text "Loading"


viewMemberSearch : SharedState -> Model -> Html Msg
viewMemberSearch sharedState model =
    let
        displaySearchResults =
            case model.searchUserForEnrollmentRequest of
                Success userEnrollment ->
                    viewUserSearchResult sharedState model <| List.head userEnrollment

                _ ->
                    text ""
    in
    rContainer <|
        [ rRow <|
            r2Column
                [ h1 [ Styles.headerStyle ] [ text "Change role" ]
                ]
                (searchElement
                    { placeholder = "Search by E-Mail"
                    , fieldType = "email"
                    , value = model.searchEnrollmentInput
                    }
                    EnrollmentSearchField
                    SetField
                    SearchUserForEnrollment
                )
        , rRow <|
            [ displaySearchResults ]
        ]


viewUserSearchResult : SharedState -> Model -> Maybe UserEnrollment -> Html Msg
viewUserSearchResult sharedState model maybeUserEnrollment =
    case maybeUserEnrollment of
        Just userEnrollment ->
            let
                user =
                    userEnrollment.user

                avatar =
                    case user.avatarUrl of
                        Just avatarUrl ->
                            avatarUrl

                        Nothing ->
                            "images/defaultAvatar.png"
            in
            div [ classes [ TC.flex, TC.flex_wrap, TC.items_center, TC.pa3, TC.ph5_l ] ]
                [ UserView.view sharedState (Tuple.first <| UserView.initFromUser user) Nothing
                    |> Html.map WriteEmailMsg
                , div [ classes [ TC.ml4_l, TC.ml0, TC.mt0_l, TC.mt2, TC.flex ] ]
                    [ multiButton
                        [ ( "Student", userEnrollment.role == Student, ChangeEnrollment { userEnrollment | role = Student } )
                        , ( "Tutor", userEnrollment.role == Tutor, ChangeEnrollment { userEnrollment | role = Tutor } )
                        , ( "Admin", userEnrollment.role == Admin, ChangeEnrollment { userEnrollment | role = Admin } )
                        ]
                    ]
                ]

        Nothing ->
            h2
                [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph3, TC.ph5_ns ]
                , Styles.listHeadingStyle
                ]
                [ text "Not found" ]


viewDetermineGroupDisplay : CourseRole -> SharedState -> Model -> Html Msg
viewDetermineGroupDisplay courseRole sharedState model =
    case model.groupModel of
        Just (BiddingModel biddingModel) ->
            rContainer <|
                [ rRowHeader "Gruppen Präferenzen"
                , BiddingView.view sharedState biddingModel
                    |> Html.map BiddingMsg
                    |> Html.map GroupMsg
                ]

        Just (DetailModel detailModel) ->
            rContainer <|
                [ rRowHeader "Übungsgruppe"
                , GroupView.view sharedState detailModel
                    |> Html.map DetailMsg
                    |> Html.map GroupMsg
                ]

        Just (GroupAdminModel adminModel) ->
            rContainer <|
                [ rRowHeader "Übungsgruppen"
                , GroupAdminView.view sharedState adminModel
                    |> Html.map GroupAdminMsg
                    |> Html.map GroupMsg
                ]

        Nothing ->
            text ""


viewSheets : SharedState -> Model -> Html Msg
viewSheets sharedState model =
    let
        baseStyle =
            [ TC.f3, TC.fw6, TC.lh_title ]

        pointsDict =
            case ( model.pointOverviewResponse, model.courseRequest ) of
                ( Success points, Success course ) ->
                    points
                        |> List.map
                            (\p ->
                                let
                                    acquiredPerc =
                                        round <|
                                            (toFloat <| p.acquired_points)
                                                / (toFloat <| p.max_points)
                                                * 100
                                in
                                ( Maybe.withDefault -1 p.sheet_id
                                , ( p.acquired_points
                                  , p.max_points
                                  , if acquiredPerc < course.required_percentage then
                                        classes (TC.red :: baseStyle)

                                    else if acquiredPerc < (course.required_percentage + 5) then
                                        classes (TC.gold :: baseStyle)

                                    else
                                        classes (TC.dark_green :: baseStyle)
                                  )
                                )
                            )
                        |> Dict.fromList

                ( _, _ ) ->
                    Dict.empty

        defaultStyle =
            classes (TC.black :: baseStyle)
    in
    rContainer <|
        [ rRowHeader "Sheets"
        , div [ classes [] ] <|
            case model.sheetRequest of
                Success sheets ->
                    [ sheets
                        |> List.sortBy (\sheet -> Time.posixToMillis sheet.due_at)
                        |> List.map
                            (\sheet ->
                                let
                                    toDisplay =
                                        case Dict.get sheet.id pointsDict of
                                            Just ( acquired, max, labelStyle ) ->
                                                ( sheet.name
                                                    ++ " - "
                                                    ++ String.fromInt acquired
                                                    ++ "/"
                                                    ++ String.fromInt max
                                                , labelStyle
                                                )

                                            Nothing ->
                                                ( sheet.name, defaultStyle )
                                in
                                { button1_icon = "get_app"
                                , button1_msg =
                                    Download <|
                                        unwrap <|
                                            sheetFile model.courseId
                                                sheet.id
                                , right_buttons =
                                    (if model.courseRole == Just Admin then
                                        [ { button_icon = "edit"
                                          , button_msg =
                                                NavigateTo <|
                                                    EditSheetRoute model.courseId
                                                        sheet.id
                                          }
                                        ]

                                     else
                                        []
                                    )
                                        ++ [ { button_icon = "arrow_forward"
                                             , button_msg =
                                                NavigateTo <|
                                                    SheetDetailRoute
                                                        model.courseId
                                                        sheet.id
                                             }
                                           ]
                                , label = Tuple.first toDisplay
                                }
                            )
                        |> nButtonList
                    ]
                        --        rRowHeaderActionButtons
                        --            (Tuple.first toDisplay)
                        --            (Tuple.second toDisplay)
                        --        <|
                        --            ([ ( "Download"
                        --               , Download <| unwrap <| sheetFile model.courseId sheet.id
                        --               , Styles.buttonGreyStyle
                        --               )
                        --             , ( "Show"
                        --               , NavigateTo <| SheetDetailRoute model.courseId sheet.id
                        --               , Styles.buttonGreyStyle
                        --               )
                        --             ]
                        --                ++ (if model.courseRole == Just Admin then
                        --                        [ ( "Edit"
                        --                          , NavigateTo <| EditSheetRoute model.courseId sheet.id
                        --                          , Styles.buttonGreyStyle
                        --                          )
                        --                        ]
                        --                    else
                        --                        []
                        --                   )
                        --            )
                        --    )
                        |> List.append
                            [ if model.courseRole == Just Admin then
                                nButtonList
                                    [ { button1_icon = "add_circle"
                                      , button1_msg =
                                            NavigateTo <|
                                                CreateSheetRoute
                                                    model.courseId
                                      , label = "Create New"
                                      , right_buttons = []
                                      }
                                    ]

                              else
                                text ""
                            ]

                _ ->
                    [ div [] [ text "Loading" ] ]
        ]


viewMaterials : SharedState -> Model -> MaterialType -> Html Msg
viewMaterials sharedState model materialType =
    let
        ( materialTypeLabel, createNewLabel ) =
            case materialType of
                Slide ->
                    ( "Folien", "Neue Folie" )

                Supplementary ->
                    ( "Material", "Neues Material" )
    in
    rContainer <|
        [ rRowHeader materialTypeLabel
        , div [ classes [] ] <|
            case model.materialRequest of
                Success materials ->
                    -- TODO stuff here
                    [ materials
                        |> List.filter (\m -> m.material_type == materialType)
                        |> List.sortBy (\m -> Time.posixToMillis m.lecture_at)
                        |> List.map
                            (\m ->
                                { button1_icon = "get_app"
                                , button1_msg =
                                    Download <|
                                        Maybe.withDefault "" <|
                                            m.file_url
                                , right_buttons =
                                    if model.courseRole == Just Admin then
                                        [ { button_icon = "edit"
                                          , button_msg =
                                                NavigateTo <|
                                                    EditMaterialRoute
                                                        model.courseId
                                                        m.id
                                          }
                                        ]

                                    else
                                        []
                                , label = m.name
                                }
                            )
                        |> nButtonList
                    ]
                        |> List.append
                            [ if model.courseRole == Just Admin then
                                nButtonList
                                    [ { button1_icon = "add_circle"
                                      , button1_msg =
                                            NavigateTo <|
                                                CreateMaterialRoute model.courseId
                                      , right_buttons = []
                                      , label = "Create New"
                                      }
                                    ]

                              else
                                text ""
                            ]

                _ ->
                    [ div [] [ text "Loading" ] ]
        ]


compareRoleName : UserEnrollment -> UserEnrollment -> Order
compareRoleName userA userB =
    case ( userA.role, userB.role ) of
        ( Admin, Admin ) ->
            compare userA.user.lastname userB.user.lastname

        ( Admin, Tutor ) ->
            LT

        ( Tutor, Admin ) ->
            GT

        ( Tutor, Tutor ) ->
            compare userA.user.lastname userB.user.lastname

        ( Tutor, _ ) ->
            LT

        ( _, Tutor ) ->
            GT

        ( _, _ ) ->
            compare userA.user.lastname userB.user.lastname


determineRole : Int -> List AccountEnrollment -> Maybe CourseRole
determineRole course_id enrollments =
    List.head <|
        List.map .role <|
            List.filter (\enrollment -> enrollment.course_id == course_id) enrollments


type Field
    = EnrollmentSearchField
    | GroupSearchField
    | ExamName Int
    | ExamDescription Int
    | ExamDate Int Time.Posix
    | ExamTime Int Time.Posix


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        EnrollmentSearchField ->
            { model | searchEnrollmentInput = value }

        GroupSearchField ->
            { model | searchGroupInput = value }

        ExamName examId ->
            let
                exams =
                    Dict.map
                        (\id e ->
                            if id == examId then
                                { e | name = value }

                            else
                                e
                        )
                        model.exams
            in
            { model | exams = exams }

        ExamDescription examId ->
            let
                exams =
                    Dict.map
                        (\id e ->
                            if id == examId then
                                { e | description = value }

                            else
                                e
                        )
                        model.exams
            in
            { model | exams = exams }

        _ ->
            -- times are handled seperately
            model
