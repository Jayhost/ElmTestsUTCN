module Main exposing (devFlags, init, main, prodFlags, reactorMain, update, view)

import Browser
import Dict exposing (update)
import Effect exposing (Effect, performEffect)
import Html exposing (Html, div, text)
import Html.Attributes exposing (href, type_)
import Model exposing (AppState(..), Config, LoadingPostsState, Mode(..), Model, Msg(..),makeModel)
import Model.Post as Post
import Model.PostIds as PostIds exposing (HackerNewsItem(..))
import Model.PostsConfig exposing (applyChanges)
import View.Posts exposing (postTable, postsConfigView)

prodFlags : Config
prodFlags =
    { apiUrl = "https://hacker-news.firebaseio.com", mode = Prod }

devFlags : Config
devFlags =
    { apiUrl = "http://localhost:3000", mode = Dev }

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init prodFlags flags |> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = subscriptions
        }

reactorMain : Program () Model Msg
reactorMain =
    Browser.element
        { init = \flags -> init devFlags flags |> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = subscriptions
        }

init : Config -> () -> ( Model, Effect )
init flags _ =
    ( Model.initModel flags
    , Effect.GetTime
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

getItems : String -> HackerNewsItem -> Effect
getItems apiUrl item =
    Effect.GetItems { apiUrl = apiUrl, item = item, onResult = GotPostIds, decoder = PostIds.decode }

getTopPostIds : String -> Effect
getTopPostIds apiUrl =
    getItems apiUrl Top

getPost : String -> Int -> Effect
getPost apiUrl postId =
    Effect.GetPost { apiUrl = apiUrl, postId = postId, onResult = GotPost, decoder = Post.decode }


uhelpr model result config time =
    case result of
    Ok (Just ids) ->
        let
            nextId = PostIds.first ids  
        in
        ( Model.LoadingPosts
            { config = config
            , time = time
            , postIds = ids
            , currentId = nextId
            , posts = []
            }
        , getPost model.config.apiUrl nextId
         )

    Ok Nothing ->
        (Model.Empty { config = config }, Effect.NoEffect)

    Err err ->
        (Model.FailedToLoad err, Effect.NoEffect)

uhelpr2 model result loading = 
    case result of
        Ok post ->
            case PostIds.advance loading.postIds of
                Just (nextId, nextPostIds) ->
                    let
                        posts = post :: loading.posts
                    in
                    if List.length posts >= loading.config.postsToFetch then
                        (Model.LoadedPosts
                            { config = loading.config
                            , time = loading.time
                            , posts = List.reverse (post :: loading.posts)
                            }
                        , Effect.NoEffect
                            )
                    else
                        (Model.LoadingPosts
                            { loading
                            | postIds = nextPostIds
                            , currentId = nextId
                            , posts = posts
                            }
                        , getPost model.config.apiUrl nextId
                            )

                Nothing ->
                            (Model.LoadedPosts
                                { config = loading.config
                                    , time = loading.time
                                    , posts = List.reverse (post :: loading.posts)
                                    }
                            , Effect.NoEffect
                            )

        Err err ->
                (Model.FailedToLoad err, Effect.NoEffect)

helpState a b =
    applyChanges b a


uhelper msg model = 
    case (model.state, msg) of
    (Model.Empty { config }, GotTime time) ->
        (Model.Loading { config = config, time = time }, getTopPostIds model.config.apiUrl)
    (Model.Loading { config, time }, GotPostIds result) -> 
        (uhelpr model result config time)
    (Model.LoadingPosts loading, GotPost result) -> 
        (uhelpr2 model result loading)
    (Model.LoadedPosts state, ConfigChanged x) -> 
        (Model.LoadedPosts
                            { config = (helpState state.config x)
                            , time = state.time
                            , posts = state.posts
                            }
                        , Effect.NoEffect
                            )
     --   ( (makeModel model.config (helpState state.config x)).state, Effect.NoEffect)
    (state, _) ->
        (state, Effect.NoEffect)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    let
        (newState, cmd) = (uhelper msg model)
    in 
        ({ model | state = newState }, cmd)   


view : Model -> Html Msg
view model =
    let
        title =
            if model.config.mode == Dev then
                "HackerNews (DEV)"
            else
                "HackerNews"

        body =
            case model.state of
                Model.Empty _ ->
                    div [] [text "Loading"]

                Model.FailedToLoad _ ->
                    div [] [text "Failed to load"]

                Model.LoadedPosts { config, time, posts } ->
                    div []
                        [ postsConfigView config
                        , postTable config time posts
                        ]

                Model.Loading _ ->
                    div [] [text "Loading stories"]

                Model.LoadingPosts { currentId } ->
                    div [] [text <| "Loading post " ++ String.fromInt currentId]

                _ ->
                    div [] [text "Other"]
    in
    div [] [Html.h1 [] [text title], body]
