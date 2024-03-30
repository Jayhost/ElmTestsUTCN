module View.Posts exposing (..)

import Html exposing (Html, div, table, tr, td, text,th,input,option,select)
import Time exposing (Posix,utc, toHour, toMinute, toSecond)
import List exposing (map)
import Html.Attributes exposing (href,type_, checked, id,selected,attribute,value,class)
import Html.Events exposing (onClick, onInput,onCheck)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time exposing (formatDuration,posixToDate,durationBetween,Duration)
import Task
--import Expect exposing (Expectation)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
toUtcString : Time.Posix -> String
toUtcString time =
  String.fromInt (toHour utc time)
  ++ ":" ++
  String.fromInt (toMinute utc time)
  ++ ":" ++
  String.fromInt (toSecond utc time)
  ++ " (UTC)"

postTable : PostsConfig -> Posix -> List Post -> Html Msg
postTable config timestamp posts =
    div []
        [ table []
            [ tr []
                [ th [] [ text "Title" ]
                , th [] [ text "Score" ]
                , th [] [ text "Type" ]
                , th [] [ text "Posted" ]
                , th [] [ text "Link" ]
                --, if config.showTimestamp then th [] [ text "Timestamp" ] else text ""
                ]
            ]
        , renderPosts config posts
        ]

renderPosts : PostsConfig -> List Post -> Html Msg
renderPosts config posts =
    table []
        (List.map (renderPost config) posts)





renderPost : PostsConfig -> Post -> Html Msg
renderPost config post =
    tr []
        [ td [class "post-title"] [ text post.title ]
        , td [class "post-score"] [ text (String.fromInt post.score) ]
        , td [class "post-time"] [ text (formatDuration ((durationBetween (Time.millisToPosix 1701716172) post.time) |> Maybe.withDefault (Duration 0 0 0 0)) ) ]
        , td [class "post-type"] [ text post.type_ ]
        , td [class "post-by"] [ text post.by ]
        , td [class "post-url"] [ text (post.url |> Maybe.withDefault "") ]
        
        ]



postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [  
          div []
            [ input [ type_ "checkbox", id "checkbox-show-job-posts", 
            checked config.showJobs, onCheck(\_ -> (ConfigChanged (ShowJobs (not config.showJobs)) ) ) ] []
            , text "Show job posts"
            ]
        , div []
            [ input [ type_ "checkbox", id "checkbox-show-text-only-posts", 
            checked config.showTextOnly, onCheck (\_->(ConfigChanged (ShowTextOnly (not config.showTextOnly))  ) ) ] []
            , text "Show text-only posts"
            ]  
        
        , div [] 
         
          [select
            [ attribute "id" "select-posts-per-page"
             , onInput (\x -> (ConfigChanged (PostsToShow (String.toInt x |> Maybe.withDefault 10))))
            ]
            [ option [ value "10", selected (config.postsToShow == 10) ] [ text "10" ]
           , option [ value "25", selected (config.postsToShow == 25) ] [ text "25" ]
           , option [ value "50", selected (config.postsToShow == 50) ] [ text "50" ]
            ]
              , div [] [ text "Posts per page" ]
              ]

          , div []
          [select
            [ attribute "id" "select-sort-by"
            , onInput (\x ->
            ConfigChanged (Sort (sortFromString x |> Maybe.withDefault None)))
            
            ]
            [ option [ value "Score", selected (config.sortBy == Score) ] [ text "Score" ]
            , option [ value "Title", selected (config.sortBy == Title) ] [ text "Title" ]
            , option [ value "Posted", selected (config.sortBy == Posted) ] [ text "Posted" ]
            , option [ value "None", selected (config.sortBy == None) ] [ text "None" ]--this needs work
          
            ]
            , div [] [ text "Sort by:" ]
            ]]


          
         
          
        
        


