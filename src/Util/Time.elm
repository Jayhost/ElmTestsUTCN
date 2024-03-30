module Util.Time exposing (..)

import Time


type Date
    = Date { year : Int, month : Time.Month, day : Int }


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


posixToDate : Time.Zone -> Time.Posix -> Date
posixToDate tz time =
    let
        year =
            Time.toYear tz time

        month =
            Time.toMonth tz time

        day =
            Time.toDay tz time
    in
    Date { year = year, month = month, day = day }


{-| Formats a `Date` instance.

    import Time

    formatDate (Date { year = 2022, month = Time.Apr, day =  4 }) {- ignore -} --> "2022 Apr 04"

    formatDate (Date { year = 2022, month = Time.Jan, day = 12 }) {- ignore -} --> "2022 Jan 12"

-}
formatDate : Date -> String
formatDate (Date date) =
    let
        year =
            String.fromInt date.year

        month =
            monthToString date.month

        day =
            String.fromInt date.day |> String.padLeft 2 '0'
    in
    year ++ " " ++ month ++ " " ++ day


formatTime : Time.Zone -> Time.Posix -> String
formatTime tz time =
    let
        date =
            posixToDate tz time

        hour =
            Time.toHour tz time |> String.fromInt |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time |> String.fromInt |> String.padLeft 2 '0'
    in
    formatDate date ++ " " ++ hour ++ ":" ++ minute


type alias Duration =
    { seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


{-| Calculates the amount of time that passed between two dates.

The first date (t1) must be **before** the second date (t2), if this not the case, the function should return `Nothing`.

Relevant library functions:

  - Use Time.posixToMillis

```
import Time

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (1000)) --> Just (Duration 1 0 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 1000)) --> Just (Duration 0 1 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 60 * 1000)) --> Just (Duration 0 0 1 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000)) --> Just (Duration 0 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000 + 1000)) --> Just (Duration 1 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (4 * 24 * 60 * 60 * 1000 + 3 * 60 * 60 * 1000 + 2 * 60 * 1000 + 1000)) --> Just (Duration 1 2 3 4)

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 0) --> Nothing

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 1000) --> Nothing
```

-}
toSecs a b =
    (Time.toSecond Time.utc a)-(Time.toSecond Time.utc b)

toMins a b =
    (Time.toMinute Time.utc a)-(Time.toMinute Time.utc b)
    
toHours a b =
    (Time.toHour Time.utc a)-(Time.toHour Time.utc b)

toDays a b =
    (Time.toDay Time.utc a)-(Time.toDay Time.utc b)

durFunc a b=
    let
        x = (toSecs a b)
        y = (toMins a b)
        w = (toHours a b)
        z = (toDays a b)
    in
    case [x,y,w,z] of
    [0,0,0,0] -> Nothing
    _ -> Just (Duration x y w z)

durationBetween : Time.Posix -> Time.Posix -> Maybe Duration
durationBetween a b = 
    if (Time.posixToMillis a)>(Time.posixToMillis b) then 
        Nothing
    else
        durFunc b a

-- durationBetween2 t1 t2 =
--     if (Time.posixToMillis t1) <= (Time.posixToMillis t2) then
--         let
--             diffMillis =
--                 Time.posixToMillis t2 - Time.posixToMillis t1

--             seconds =
--                 diffMillis // 1000

--             minutes =
--                 seconds // 60

--             hours =
--                 minutes // 60

--             days =
--                 hours // 24
--         in
--         Just (Duration seconds minutes hours days)
--     else
--         Nothing


-- durationBetween2 :: Time.Posix -> Time.Posix -> Maybe Duration
-- modBy :: Int -> Int -> Int
modBy a b = a - (b * (a // b))

-- durationBetween2 :: Time.Posix -> Time.Posix -> Maybe Duration
durationBetween2 t1 t2 =
    if Time.posixToMillis t1 < Time.posixToMillis t2 then
        let
            diffMillis = Time.posixToMillis t2 - Time.posixToMillis t1

            secondsTotal = diffMillis // 1000

            days = secondsTotal // 86400 
            remainingSeconds = modBy secondsTotal 86400

            hours = remainingSeconds // 3600 
            remainingSeconds2 = modBy remainingSeconds 3600

            minutes = remainingSeconds2 // 60 
            seconds = remainingSeconds2 // 60 
        in
        Just (Duration days hours minutes seconds)
    else
        Nothing

    
       
        
        
        
{-
durationBetween : Time.Posix -> Time.Posix -> Maybe Duration
durationBetween a b = 
    let diff = Just (Duration (toSecs x y) (toMins x y) (toHours x y) (toDays x y) )
    in
    case diff of
    (Duration 0 0 0 0) -> Nothing
    _ -> diff
-}
{-| Format a `Duration` as a human readable string

    formatDuration (Duration 1 0 0 0) --> "1 second ago"

    formatDuration (Duration 2 0 0 0) --> "2 seconds ago"

    formatDuration (Duration 0 1 0 0) --> "1 minute ago"

    formatDuration (Duration 0 0 2 0) --> "2 hours ago"

    formatDuration (Duration 0 0 0 3) --> "3 days ago"

    formatDuration (Duration 0 1 1 1) --> "1 day 1 hour 1 minute ago"

    formatDuration (Duration 0 47 6 2) --> "2 days 6 hours 47 minutes ago"

    formatDuration (Duration 0 30 0 1) --> "1 day 30 minutes ago"

-}
--formatDuration : Duration -> String
--formatDuration : Duration -> Lis

emptyZero a b = 
    if b == 1 then
    String.fromInt(b) ++ a ++ " "
    else if b /= 0 then
    String.fromInt(b) ++ a ++ "s "
    else
    ""

formatDuration dur =
    let 
        {seconds,minutes,hours,days} = dur
        dayz = emptyZero " day" days
        hourz = emptyZero " hour" hours
        minz = emptyZero " minute" minutes
        secz = emptyZero " second" seconds
    in
    dayz ++ hourz ++ minz ++ secz ++ "ago"


    
       
        
    
    --case [days,hours,minutes,seconds] of
    --[x,y,z,0] -> String.fromInt(x) ++ " day " ++ String.fromInt(y) ++ " hours ago " ++ String.fromInt(z) ++ " minutes ago"
    --[x,y,0,0] -> String.fromInt(days) ++ " day " ++ String.fromInt(hours) ++ " hours ago " 
    --[x,0,z,0] -> String.fromInt(days) ++ " day " ++ String.fromInt(minutes) ++ " minutes ago"
