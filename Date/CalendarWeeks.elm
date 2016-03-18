module Date.CalendarWeeks (
    CWSystem(..)
  , CalendarWeek
  , cwFromYearAndWeek
  , cwYear
  , cwWeek
  , daysInYearWithCalendarWeek
  , calendarWeeksInYear
  , calendarWeeksWithDaysInYear
  , whichCalendarWeek
  , daysInCalendarWeek
  , weekDays
  , toISO8601WeekString
  , toISO8601WeekDateString
  , fromISO8601WeekString
  , fromISO8601WeekDateString
  ) where

{-| This library deals with calculations regardings calendar weeks, i.e.
it can tell you which calendar weeks are present in a given year, which
calendar week corresponds to a certain date, etc.

Calendar week numbering depends on which calendar system you are using.
The three main systems, all of which are supported by this library are ISO-8601,
the Islamic system, and the North American system.

ISO-8601 is the most common system in Europe and Asia. It defines the first
calendar week of a year to be the one containing January 4th. Days of the week
range from Monday to Sunday. ISO-8601 defines a format for week and date
strings which are also supported by this library.

The Islamic system defines the first calendar week of a year to be the one
containing January 1st. Its week days range from Saturday to Friday.

Similarly, the North American system (which besides in the US and Canada is 
used in China, Japan, Israel, South Africa, and most of Latin America) 
also starts with January 1st but the days of the week range from Sunday to
Saturday.

# Types, Constructors and Deconstructors
@docs CWSystem, CalendarWeek, cwFromYearAndWeek, cwYear, cwWeek

# Lists of days and calendar weeks per year

One use case of this library is to get a combination of dates in a year
and calendar weeks, or the other way round: calendar weeks in a year
with the corresponding dates.

@docs daysInYearWithCalendarWeek, calendarWeeksWithDaysInYear, calendarWeeksInYear

# Determining calendar weeks and corresponding days

Another use case is that you have a date and want to determine the
corresponding calendar week, or the other way round: you have a calendar
week and want to determine which days are part of it.

@docs whichCalendarWeek, daysInCalendarWeek, weekDays

# ISO-8601

ISO-8601 defines string formats for calendar weeks and dates based
on calendar weeks and the day of the week. This library supports converting
both to and from these string formats.

@docs toISO8601WeekString, fromISO8601WeekString, toISO8601WeekDateString, fromISO8601WeekDateString
-}

import Date exposing (..)
import Maybe exposing (withDefault)

import Result
import String
import List.Extra


{-| A data type for the possible calendar week systems for use in this library.

For their definition, please refer to the Wikipedia page on
week numbering (https://en.wikipedia.org/wiki/Week#Week_numbering).
-}
type CWSystem =
    ISO8601
  | Islamic
  | NorthAmerican


{-| A data type to represent a calendar week, i.e. a year and week number.
-}
type CalendarWeek =
    CalendarWeek {
          year : Int
        , week : Int
        }


{-| Given a year and week number, construct a CalendarWeek.

    cwFromYearAndWeek 2016 51
-}
cwFromYearAndWeek : Int -> Int -> CalendarWeek
cwFromYearAndWeek year week =
    CalendarWeek { year = year, week = week }


{-| Given a CalendarWeek, extract the year.

    cwYear <| cwFromYearAndWeek 2016 51 == 2016
-}
cwYear : CalendarWeek -> Int
cwYear calendarWeek =
    case calendarWeek of
        CalendarWeek record -> record.year


{-| Given a CalendarWeek, extract the month.

    cwMonth <| cwFromYearAndWeek 2016 51 == 51
-}
cwWeek : CalendarWeek -> Int
cwWeek calendarWeek =
    case calendarWeek of
        CalendarWeek record -> record.week


daysInYear : Int -> List Date
daysInYear year =
    let
        daysInMonth : Int -> Int -> List Date
        daysInMonth y m =
            let 
                isLeapYear : Int -> Bool
                isLeapYear year =
                    if (year % 4) /= 0 then
                       False
                    else if (year % 100) /= 0 then
                       True
                    else if (year % 400) /= 0 then
                       False
                    else
                       True


                daysPerMonth : Int -> Int -> Int
                daysPerMonth year month =
                    case month of
                        1 -> 31
                        2 -> (if isLeapYear year then 29 else 28)
                        3 -> 31
                        4 -> 30
                        5 -> 31
                        6 -> 30
                        7 -> 31
                        8 -> 31
                        9 -> 30
                        10 -> 31
                        11 -> 30
                        12 -> 31
                        _  -> 0
            in
                List.filterMap (\d -> maybeDate y m d) [1..daysPerMonth y m]
    in
        List.concat <| List.map (\month -> daysInMonth year month) [1..12]


maybeDate : Int -> Int -> Int -> Maybe Date
maybeDate y m d =
    let
        dayToDateString : Int -> Int -> Int -> String
        dayToDateString year month day =
            toString year ++ "/" ++ toString month ++ "/" ++ toString day
    in
    dayToDateString y m d |> Date.fromString |> Result.toMaybe


firstDayOfWeek : CWSystem -> Day
firstDayOfWeek cws =
    case cws of
        ISO8601 ->
            Mon
        Islamic ->
            Sat
        NorthAmerican ->
            Sun


{-| Given a CWSystem and a year, produce a list of tuples of
date and corresponding CalendarWeek for each day of the year.

    let
        dateToString d =
            toString (Date.month d) ++ " " ++ toString (Date.day d)
    in
       daysInYearWithCalendarWeek ISO8601 2016 |>
       List.map (\(d, cw) -> (dateToString d) ++ ": "
                          ++ (toISO8601WeekString cw)) |>
       List.take 5

evaluates to

    ["Jan 1: 2015-W53","Jan 2: 2015-W53","Jan 3: 2015-W53",
    "Jan 4: 2016-W01","Jan 5: 2016-W01"]
-}
daysInYearWithCalendarWeek : CWSystem -> Int -> List (Date, CalendarWeek)
daysInYearWithCalendarWeek cws y =
    let
        -- a list where every first day of the week in the given
        -- calendar system has a 1, everything else has a 0
        firstDayOnes =
            List.indexedMap (\i b -> if i > 0 && b then 1 else 0) <|
            List.map (\d -> dayOfWeek d == firstDayOfWeek cws) <| daysInYear y 

        -- the summed up version of this starting at the corresponding
        -- start week of the given year (0 meaning the last week of the
        -- year before)
        calendarWeeks year =
            let
                -- compute the start week for the first day of the year
                -- (either 0 or 1)
                startWeek : CWSystem -> Int -> Int
                startWeek cws y =
                    case cws of
                        -- in ISO8601, January 1st can be in the
                        -- last week of the previous year (called
                        -- week "0" here)
                        ISO8601 ->
                            let
                                firstDay =
                                    case (maybeDate y 1 1) of
                                        Nothing ->
                                            Mon -- shouldn't happen
                                        Just d  ->
                                            dayOfWeek d
                            in
                               case firstDay of
                                   Mon -> 1
                                   Tue -> 1
                                   Wed -> 1
                                   Thu -> 1
                                   _   -> 0
                        -- all other systems start with January 1st
                        -- being the start of the first week
                        _ ->
                            1
            in
                List.scanl (+) (startWeek cws year) firstDayOnes

        -- the last calendar week of the previous year
        lastWeekPrevYear : Int -> CalendarWeek
        lastWeekPrevYear year' =
            CalendarWeek { 
                year = (year' - 1)
              , week = withDefault 0 (List.Extra.last <|
                                      calendarWeeks (year' - 1))
            }

        -- calculates whether the last week is supposed to count as
        -- the first week of the next year based on calendar system
        -- and the length of the days of this calendar year in the
        -- last week
        isLastWeekCWOfNextYear : CWSystem -> Int -> Bool
        isLastWeekCWOfNextYear cws length =
            case cws of
                ISO8601 ->
                    length <= 3 -- in order for Jan 4th to be in it
                _ ->
                    length <= 6 -- in order for Jan 1st to be in it

        -- a "fixed" version of the list where the "0" week is replaced by
        -- the corresponding last calendar week from the previous year
        fixFirstWeek =
            List.map (\w -> if w == 0 then
                               lastWeekPrevYear y
                            else
                               CalendarWeek { year = y, week = w })

        -- fix the last n days, since they might already be in the first
        -- week of the next year. This highly depends on the chosen CWSystem
        fixLastNDays cws days =
            let
                lastDayDOW = case (maybeDate y 12 31) of
                    Nothing -> Mon -- shouldn't happen :-/
                    Just d  -> Date.dayOfWeek d
                daysToFix cws = case cws of
                    ISO8601 ->
                        case lastDayDOW of
                            Mon -> 1
                            Tue -> 2
                            Wed -> 3
                            Thu -> 0
                            Fri -> 0
                            Sat -> 0
                            Sun -> 0
                    NorthAmerican ->
                        case lastDayDOW of
                            Sun -> 1
                            Mon -> 2
                            Tue -> 3
                            Wed -> 4
                            Thu -> 5
                            Fri -> 6
                            Sat -> 0
                    Islamic ->
                        case lastDayDOW of
                            Sat -> 1
                            Sun -> 2
                            Mon -> 3
                            Tue -> 4
                            Wed -> 5
                            Thu -> 6
                            Fri -> 0
            in
                List.take ((List.length days) - (daysToFix cws)) days
             ++ (List.map (\(d, cw) -> (d, cwFromYearAndWeek (y+1) 1))
                <| List.drop ((List.length days) - (daysToFix cws)) days)

    in
       fixLastNDays cws <| List.map2 (,) (daysInYear y) <| List.drop 1 
       <| fixFirstWeek (calendarWeeks y)


{-| Given a CWSystem and a year, produce a list of calendar weeks and a
corresponding list of days in those calendar weeks. The result contains
all calendar weeks which contain days from the given year, which might
include calendar weeks (and thus also dates) belonging to the year before
or after. I.e.,

    Maybe.map (\(cw, _) -> toISO8601WeekString cw) <|
        List.head <| calendarWeeksWithDaysInYear ISO8601 2016

evaluates to

    Just "2015-W53"

Similarly,

    Maybe.map (\(cw, _) -> toISO8601WeekString cw) <|
        List.Extra.last <| calendarWeeksWithDaysInYear ISO8601 2018

evaluates to

    Just "2019-W01"
-}
calendarWeeksWithDaysInYear : CWSystem -> Int -> List (CalendarWeek, (List Date))
calendarWeeksWithDaysInYear cws year =
    daysInYearWithCalendarWeek cws year |> List.map snd |>
    List.Extra.group |> List.filterMap List.head |>
    List.map (\cw -> (cw, daysInCalendarWeek cws cw))


{-| Given a CWSystem and a year, produces a list of tuples of CalendarWeek and
a list of corresponding Dates from that year.
Note that unlike with calendarWeeksWithDaysInYear, this might mean that some
dates from this calendar year are not present in the list of dates.
-}
calendarWeeksInYear : CWSystem -> Int -> List (CalendarWeek, (List Date))
calendarWeeksInYear cws year =
    List.filter (\e -> cwYear (fst e) == year) <| calendarWeeksWithDaysInYear cws year


{-| Given a CWSystem, produces the seven weekdays in the order applicable
for that calendar week system.
-}
weekDays : CWSystem -> List Date.Day
weekDays cws =
    case cws of
        ISO8601 ->
            [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]
        NorthAmerican ->
            [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
        Islamic ->
            [ Sat, Sun, Mon, Tue, Wed, Thu, Fri ]


{-| Given a CWSystem and a date, return the CalendarWeek that contains this date.
    Maybe.map (cwWeek << whichCalendarWeek ISO8601)
        (Result.toMaybe <| Date.fromString "2016/03/14") == Just 11
    Maybe.map (cwWeek << whichCalendarWeek Islamic d)
        (Result.toMaybe <| Date.fromString "2016/03/14") == Just 12
-}
whichCalendarWeek : CWSystem -> Date -> CalendarWeek
whichCalendarWeek cws d =
    let
        datesEqual d e =
            Date.year d == Date.year e
         && Date.month d == Date.month e
         && Date.day d == Date.day e
    in
    withDefault
        (CalendarWeek { week = 0  -- shouldn't happen
                     , year = 0 })
        (List.head <| List.map (\e -> snd e) <|
            List.filter (\e -> datesEqual (fst e) d)
              (daysInYearWithCalendarWeek cws (Date.year d)))


{-| Given a CWSystem and a CalendarWeek, return a list of dates in that
calendar week.

    daysInCalendarWeek ISO8601 (cwFromYearAndWeek 2016 1)
    |> List.map (\d -> (Date.month d, Date.day d))

evaluates to

    [(Jan,4),(Jan,5),(Jan,6),(Jan,7),(Jan,8),(Jan,9),(Jan,10)]


While

    daysInCalendarWeek NorthAmerican (cwFromYearAndWeek 2016 1)
    |> List.map (\d -> (Date.month d, Date.day d))

evaluates to

    [(Dec,27),(Dec,28),(Dec,29),(Dec,30),(Dec,31),(Jan,1),(Jan,2)]

And

    daysInCalendarWeek Islamic (cwFromYearAndWeek 2016 1)
    |> List.map (\d -> (Date.month d, Date.day d))

evaluates to

    [(Dec,26),(Dec,27),(Dec,28),(Dec,29),(Dec,30),(Dec,31),(Jan,1)]
-}
daysInCalendarWeek : CWSystem -> CalendarWeek -> List Date
daysInCalendarWeek cws cw =
    List.map (fst) <| List.filter (\e -> snd e == cw) <|
        -- TODO could be optimized by looking at prev/next year only if necessary
           (daysInYearWithCalendarWeek cws ((cwYear cw) - 1))
        ++ (daysInYearWithCalendarWeek cws (cwYear cw))
        ++ (daysInYearWithCalendarWeek cws ((cwYear cw) + 1))


{-| Given a CalendarWeek, convert it into an ISO8601 week string of
format YYYY-Www.

    (toISO8601WeekString <| cwFromYearAndWeek 2016 3) == "2016-W03"
-}
toISO8601WeekString : CalendarWeek -> String
toISO8601WeekString calendarWeek =
    (toString <| cwYear calendarWeek) ++ "-W"
 ++ (if cwWeek calendarWeek < 10 then "0" else "")
 ++ (toString <| cwWeek calendarWeek)


{-| Given a Date, convert it into an ISO8601 week date string of
format YYYY-Www-d.

    Maybe.map (toISO8601WeekDateString)
        (Result.toMaybe <| Date.fromString "1979/06/26")

evaluates to

    Just "1979-W26-2"
-}
toISO8601WeekDateString : Date -> String
toISO8601WeekDateString date =
    let
        cw = whichCalendarWeek ISO8601 date
        dayOfWeekNr d =
            case d of
                Mon -> 1
                Tue -> 2
                Wed -> 3
                Thu -> 4
                Fri -> 5
                Sat -> 6
                Sun -> 7
    in
       (toISO8601WeekString cw) ++ "-"
    ++ (toString <| dayOfWeekNr <| dayOfWeek date)
    

{-| Given an ISO8601 week string of format YYYY-Www or YYYYWww,
return a Just CalendarWeek if parsing succeeds or Nothing if it fails.
-}
fromISO8601WeekString : String -> Maybe CalendarWeek
fromISO8601WeekString weekString =
     let
         maybeYear =
             Result.toMaybe <| String.toInt <| String.slice 0 4 weekString
         weekOffset =
             case (String.slice 4 5 weekString) of
                 "-" -> 6 -- YYYY-Www
                 _ -> 5   -- YYYYWww
         maybeWeek =
             Result.toMaybe <| String.toInt <|
                String.slice weekOffset (weekOffset + 2) weekString
    in
        case maybeYear of
            Nothing -> Nothing
            Just y  -> case maybeWeek of
                Nothing -> Nothing
                Just w  -> Just (CalendarWeek { year = y, week = w })


{-| Given an ISO8601 week date string of format YYYY-Www-d or YYYYWwwd,
return a Just Date if parsing succeeds or Nothing if it fails.
-}
fromISO8601WeekDateString : String -> Maybe Date
fromISO8601WeekDateString weekDateString =
    let
        -- weekDateString can either be YYYY-Www-d or
        -- YYYYWwwd. In the second case, normalize it
        -- to be with dashes (the output format of
        -- toISO8601WeekDateString), so we can just look
        -- for dates in that year which produce the corresponding
        -- output
        -- TODO this is quite slow, could be optimized
        dashString =
            case String.length weekDateString of
                8 -> (String.slice 0 4 weekDateString) ++ "-W"
                  ++ (String.slice 5 7 weekDateString) ++ "-"
                  ++ (String.slice 7 8 weekDateString)
                _ -> weekDateString
        maybeYear =
            Result.toMaybe <| String.toInt <| String.slice 0 4 weekDateString
    in
        List.head <|
            List.filter (\d -> toISO8601WeekDateString d == dashString) <|
            daysInYear (withDefault 0 maybeYear)
