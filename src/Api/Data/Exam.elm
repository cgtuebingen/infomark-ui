module Api.Data.Exam exposing
    ( Exam
    , ExamEnrollment
    , ExamEnrollments
    , Exams
    , enrollmentDecoder
    , enrollmentEncoder
    , enrollmentsDecoder
    , examDecoder
    , examEncoder
    , examsDecoder
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Time exposing (Posix)


type alias Exam =
    { id : Int
    , name : String
    , description : String
    , exam_time : Posix
    , course_id : Int
    }


type alias Exams =
    List Exam


type alias ExamEnrollment =
    { status : Int
    , mark : String
    , user_id : Int
    , course_id : Int
    , exam_id : Int
    }


type alias ExamEnrollments =
    List ExamEnrollment


examDecoder : Decoder Exam
examDecoder =
    Decode.succeed Exam
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "exam_time" Iso8601.decoder
        |> required "course_id" Decode.int


examEncoder : Exam -> Encode.Value
examEncoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "exam_time", Iso8601.encode model.exam_time )
        , ( "description", Encode.string model.description )
        , ( "course_id", Encode.int model.course_id )
        ]


examsDecoder : Decoder Exams
examsDecoder =
    Decode.list examDecoder


enrollmentDecoder : Decoder ExamEnrollment
enrollmentDecoder =
    Decode.succeed ExamEnrollment
        |> required "status" Decode.int
        |> required "mark" Decode.string
        |> required "user_id" Decode.int
        |> required "course_id" Decode.int
        |> required "exam_id" Decode.int


enrollmentsDecoder : Decoder ExamEnrollments
enrollmentsDecoder =
    Decode.list enrollmentDecoder


enrollmentEncoder : ExamEnrollment -> Encode.Value
enrollmentEncoder model =
    Encode.object
        [ ( "status", Encode.int model.status )
        , ( "mark", Encode.string model.mark )
        , ( "user_id", Encode.int model.user_id )
        , ( "course_id", Encode.int model.course_id )
        , ( "exam_id", Encode.int model.exam_id )
        ]
