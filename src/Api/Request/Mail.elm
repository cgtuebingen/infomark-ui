module Api.Request.Mail exposing (sendCourseMailPost, sendGroupMailPost)

import Api.Data.Mail as Mail exposing (Mail)
import Api.Endpoint exposing (courseEmail, groupEmail, unwrap)
import Api.Helper exposing (post, postExpectNothing)
import Http
import RemoteData exposing (RemoteData(..), WebData)


sendCourseMailPost : Int -> Mail -> (WebData () -> msg) -> Cmd msg
sendCourseMailPost courseId mailObject msg =
    postExpectNothing (unwrap <| courseEmail courseId)
        (Http.jsonBody <| Mail.encoder mailObject)
        msg


sendGroupMailPost : Int -> Int -> Mail -> (WebData () -> msg) -> Cmd msg
sendGroupMailPost courseId groupId mailObject msg =
    postExpectNothing (unwrap <| groupEmail courseId groupId)
        (Http.jsonBody <| Mail.encoder mailObject)
        msg
