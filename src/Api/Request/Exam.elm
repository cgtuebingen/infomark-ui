module Api.Request.Exam exposing
    ( examDelete
    , examEnrollmentDelete
    , examEnrollmentGet
    , examEnrollmentPost
    , examEnrollmentPut
    , examGet
    , examPost
    , examPut
    , examsGet
    )

import Api.Data.Exam
    exposing
        ( Exam
        , ExamEnrollment
        , Exams
        , enrollmentDecoder
        , enrollmentEncoder
        , examDecoder
        , examEncoder
        , examsDecoder
        )
import Api.Endpoint exposing (courseExams, exam, examEnrollment, unwrap)
import Api.Helper
    exposing
        ( deleteExpectNothing
        , get
        , post
        , postExpectNothing
        , postFile
        , putExpectNothing
        )
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


examsGet : Int -> (WebData Exams -> msg) -> Cmd msg
examsGet courseId msg =
    get (unwrap <| courseExams courseId)
        msg
    <|
        examsDecoder


examPost : Int -> Exam -> (WebData () -> msg) -> Cmd msg
examPost courseId examNew msg =
    postExpectNothing (unwrap <| courseExams courseId)
        (Http.jsonBody (examEncoder examNew))
        msg


examGet : Int -> Int -> (WebData Exam -> msg) -> Cmd msg
examGet courseId examId msg =
    get (unwrap <| exam courseId examId)
        msg
    <|
        examDecoder


examPut : Int -> Int -> Exam -> (WebData () -> msg) -> Cmd msg
examPut courseId examId examNew msg =
    putExpectNothing (unwrap <| exam courseId examId)
        (Http.jsonBody <| examEncoder examNew)
        msg


examDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
examDelete courseId examId msg =
    deleteExpectNothing (unwrap <| exam courseId examId) msg


examEnrollmentPost : Int -> Int -> (WebData () -> msg) -> Cmd msg
examEnrollmentPost courseId examId msg =
    postExpectNothing (unwrap <| examEnrollment courseId examId)
        Http.emptyBody
        msg


examEnrollmentDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
examEnrollmentDelete courseId examId msg =
    deleteExpectNothing (unwrap <| examEnrollment courseId examId) msg


examEnrollmentGet : Int -> Int -> (WebData ExamEnrollment -> msg) -> Cmd msg
examEnrollmentGet courseId examId msg =
    get (unwrap <| examEnrollment courseId examId)
        msg
    <|
        enrollmentDecoder


examEnrollmentPut : Int -> Int -> ExamEnrollment -> (WebData () -> msg) -> Cmd msg
examEnrollmentPut courseId examId enrollmentNew msg =
    putExpectNothing (unwrap <| examEnrollment courseId examId)
        (Http.jsonBody (enrollmentEncoder enrollmentNew))
        msg
