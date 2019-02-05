module UtilsTester exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import EmailHelper

success : Result a b -> Bool
success result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False

suite : Test
suite =
    describe "Test University email"
        [ test "Student email" <|
            \_ -> EmailHelper.check_university_mail(
                    "max.mustermann@student.uni-tuebingen.de") 
                |> success
                |> Expect.true "Expected a student email to be valid"
                
        , test "Member email" <|
            \_ -> EmailHelper.check_university_mail(
                    "max.mustermann@uni-tuebingen.de") 
                |> success
                |> Expect.true "Expected a university email to be valid"
        , test "Other email" <|
            \_ -> EmailHelper.check_university_mail(
                    "max.mustermann@gmail.com") 
                |> success
                |> Expect.false "Expected other email addresses to fail"
        , test "Numbers allowed" <|
            \_ -> EmailHelper.check_university_mail(
                    "max.mustermann1@uni-tuebingen.de") 
                |> success
                |> Expect.true "Expected a mail address with numbers to be valid"
        , test "Hyphens allowed" <|
            \_ -> EmailHelper.check_university_mail(
                    "max-mustermann@uni-tuebingen.de") 
                |> success
                |> Expect.true "Expected a mail address with a hyphen to be valid"
        ]
