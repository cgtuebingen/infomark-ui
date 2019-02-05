module EmailHelper exposing(check_university_mail)

import Regex

check_university_mail : String -> Result String String
check_university_mail email =
    let 
        emailPattern = Maybe.withDefault Regex.never <|
            Regex.fromString "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]*uni-tuebingen.de"
        
        isValidEmail =
            Regex.contains emailPattern email
    in
        if isValidEmail then
            Ok "The email address is valid!"
        else
            Err "Only University of Tübingen email addresses are allowed!"