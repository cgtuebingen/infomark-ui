module Utils.EmailHelper exposing (check_university_mail, isInvalid)

import Regex
import Validate exposing (Validator, ifTrue)


isInvalid : String -> Bool
isInvalid email =
    let
        emailPattern =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]*uni-tuebingen.de"
    in
    not <| Regex.contains emailPattern email


check_university_mail : String -> Result String String
check_university_mail email =
    let
        isInvalidEmail =
            isInvalid email
    in
    if isInvalidEmail then
        Err "Only University of Tübingen email addresses are allowed!"

    else
        Ok "The email address is valid!"


ifUniversityEmail : (subject -> String) -> error -> Validate.Validator error subject
ifUniversityEmail subjectToString error =
    ifTrue (\subject -> isInvalid (subjectToString subject)) error
