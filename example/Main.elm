import Html exposing (div, button, text, input, p, ul, li, node, h1)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import StartApp.Simple as StartApp
import Date exposing (..)
import Debug
import Date.CalendarWeeks exposing (..)
import String
import Effects

main =
    StartApp.start { model = model currentISODate, view = view, update = update }

port currentISODate : String

type alias Model =
    {
      date : Maybe Date
    , currentYear : Maybe Int
    , calendarWeek : Maybe CalendarWeek
    , cwSystem : CWSystem
    , cwTable : List (CalendarWeek, List Date)
    }

type Action =
        ChangeCWSystem CWSystem
      | IncreaseYear
      | DecreaseYear

-- a little blurp about which day it is, ISO-weekdate for ISO8601,
-- just naming the calendar week for the rest
dateCWBlurp cws mDate =
    let
        isoDate d =
            (toString <| year d) ++ "-" ++ (monToString <| month d)
                                 ++ "-" ++ (toString <| day d)
    in
        case mDate of
            Nothing -> ""
            Just d ->
                case cws of
                    ISO8601 ->
                        "Today is " ++ (isoDate d) ++ ", also known as "
                        ++ (toISO8601WeekDateString d) ++ "."
                    _ ->
                        "Today is " ++ (isoDate d) ++ ", a day in calendar week "
                        ++ (toString <| cwWeek <| whichCalendarWeek cws d) ++ "."


-- big text showing the current calendar week
showMaybeCW : Maybe CalendarWeek -> String
showMaybeCW mcw =
    Maybe.withDefault "" <| Maybe.map
        (\cw -> "Today's calendar week: " ++ (toString <| cwWeek cw)) mcw


-- the main calendar week table for a year, highlighting the current week
-- TODO: highlight current date?
showMaybeCWYearTable cws my table mcw =
    case my of
        Nothing -> text ""
        Just y  -> Html.table [ class "table table-hover" ] <| [
            Html.thead [] [
                Html.tr [] <| [
                    Html.th [] [ text "CW" ]
                ]
                ++ (List.map (\d -> Html.th [] [ text <| toString d ])
                   <| weekDays cws)
            ] 
          , Html.tbody [] (List.map (\e -> cwEntryToTr e mcw) <| table)
        ]


monToString : Date.Month -> String
monToString d =
    case d of
        Jan -> "01"
        Feb -> "02"
        Mar -> "03"
        Apr -> "04"
        May -> "05"
        Jun -> "06"
        Jul -> "07"
        Aug -> "08"
        Sep -> "09"
        Oct -> "10"
        Nov -> "11"
        Dec -> "12"


-- zero-padding if necessary.
dayToString : Int -> String
dayToString d =
    let
        str = toString d
    in
        if (String.length str) == 1 then
           "0" ++ str
        else
            str

-- zero-padding if necessary, TODO: refactor
cwToString : CalendarWeek -> String
cwToString cw =
    let
        str = toString <| cwWeek cw
    in
       if (String.length str) == 1 then
          "0" ++ str
       else
          str


-- one calendar week to one table row
cwEntryToTr (cw, dates) mcw =
    Html.tr [
        class (case mcw of
            Nothing -> ""
            Just c  -> if (c == cw) then
                          "info" -- current week
                       else
                          "")
        ] <| [
            Html.td [] [ text <| cwToString cw ]
        ]
    ++ (List.map (\d -> Html.td [] [ text <|
        (dayToString <| Date.day d) ++ "."
        ++ (monToString <| Date.month d) ++ "."
        ])
        dates)


computeCalendarWeek : CWSystem -> Maybe Date -> Maybe CalendarWeek
computeCalendarWeek cws = Maybe.map (whichCalendarWeek cws)


-- navigation bar with choice of years and calendar week system
navBar address currYear prevYear nextYear cws =
    let
        isActive cws1 cws2 = -- current calendar week system
            if cws1 == cws2 then
               "active"
            else 
               ""
        navBarYears =
            ul [ class "nav navbar-nav" ] [
                li [] []
              , li [] [
                    Html.a [ onClick address DecreaseYear ] [ text prevYear ]
                ]  
              , li [ class "active" ] [
                    Html.a [ ] [ text currYear ]
                ]  
              , li [] [
                  Html.a [ onClick address IncreaseYear ] [ text nextYear ]
                ]  
            ]
        navBarCWS =
            ul [ class "nav navbar-nav navbar-right" ] [
                  li [ class (isActive ISO8601 cws) ] [
                      Html.a [ onClick address <| ChangeCWSystem ISO8601 ]
                             [ text "ISO-8601" ]
                  ]
                , li [ class (isActive NorthAmerican cws) ] [
                      Html.a [ onClick address <| ChangeCWSystem NorthAmerican ]
                             [ text "North American" ]
                  ]
                , li [ class (isActive Islamic cws) ] [
                      Html.a [ onClick address <| ChangeCWSystem Islamic ]
                             [ text "Islamic" ]
                  ]
            ]  
    in
        node "nav" [ class "navbar navbar-default navbar-static-top" ] [
            div [ class "container" ] [
                div [ class "navbar-header" ] [
                    Html.a [ class "navbar-brand" ] [ text "Calendar weeks" ]
                ]
              , div [ id "navbar", class "navbar-collapse collapse" ] [
                  navBarYears
                , navBarCWS  
                ]
            ]
        ]


-- jumbotron containing the current date and calendar week
currentDateAndWeekJumbotron model =
    div [ class "jumbotron" ] [
        h1 [] [ text (showMaybeCW model.calendarWeek) ]
      , p [] [
            text <| dateCWBlurp model.cwSystem model.date
        ]
    ]


calendarWeekTable model =
    div [ class "row" ] [
        (showMaybeCWYearTable model.cwSystem model.currentYear
                              model.cwTable model.calendarWeek)
    ]


footer =
    node "footer" [ class "footer" ] [
        p [] [
              text "Made with "
             , Html.a [ href "http://www.elm-lang.org" ] [ text "elm" ]
             , text ". Uses "
             , Html.a [ href "https://github.com/alech/elm-calendarweeks" ] [text "elm-calendarweeks" ]
             , text " by "
             , Html.a [ href "https://www.twitter.com/alech" ] [text "@alech"]
             , text "."
        ]
    ]


-- MVC below

-- initialize based on the ISO date string passed in from JS
model : String -> Model
model dateStr =
    let
        maybeDate = Result.toMaybe <| Date.fromString dateStr
        maybeCurrYear = Maybe.map year maybeDate
    in
        {
            date = maybeDate
          , currentYear = maybeCurrYear
          , calendarWeek = computeCalendarWeek ISO8601 maybeDate
          , cwSystem = ISO8601
          , cwTable = Maybe.withDefault [] <|
                Maybe.map (calendarWeeksWithDaysInYear ISO8601) maybeCurrYear
        }


-- build view based on helper functions
view address model =
    let
        currYear = Maybe.withDefault "" <| Maybe.map toString model.currentYear
        prevYear = Maybe.withDefault "" <| Maybe.map (\y -> toString <| y - 1) model.currentYear
        nextYear = Maybe.withDefault "" <| Maybe.map (\y -> toString <| y + 1) model.currentYear
    in
        div [] [
            navBar address currYear prevYear nextYear model.cwSystem
          , div [ class "container" ] [
                currentDateAndWeekJumbotron model
              , calendarWeekTable model
              , footer
            ]
        ]


-- different ways of updating the model (changing year or calendar week system)
update action model =
  case action of
    ChangeCWSystem cws ->
        { model | cwSystem = cws
                , cwTable = Maybe.withDefault []
                          (Maybe.map (calendarWeeksWithDaysInYear cws) model.currentYear)
                , calendarWeek = computeCalendarWeek cws model.date
        }
    IncreaseYear ->
        let
            nextYear = Maybe.map (\y -> y + 1) model.currentYear
        in
            { model | currentYear = nextYear
                    , cwTable = Maybe.withDefault []
                              (Maybe.map (calendarWeeksWithDaysInYear model.cwSystem) nextYear)
            }
    DecreaseYear ->
        let
            prevYear = Maybe.map (\y -> y - 1) model.currentYear
        in
            { model | currentYear = prevYear
                    , cwTable = Maybe.withDefault []
                              (Maybe.map (calendarWeeksWithDaysInYear model.cwSystem) prevYear)
            }
