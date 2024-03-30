module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


sortFromString : String -> Maybe SortBy
sortFromString sort =
    case sort of
        "Score" ->
            Just Score

        "Title" ->
            Just Title

        "Posted" ->
            Just Posted

        "None" ->
            Just None
        _ -> Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True




type Change 
    = PostsToFetch Int
    | PostsToShow Int
    | Sort SortBy
    | ShowJobs Bool
    | ShowTextOnly Bool


applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges a b =
    case a of 
        PostsToFetch x -> (PostsConfig x b.postsToShow b.sortBy b.showJobs b.showTextOnly)
        PostsToShow x -> (PostsConfig b.postsToFetch x b.sortBy b.showJobs b.showTextOnly)
        Sort x -> (PostsConfig b.postsToFetch b.postsToShow x b.showJobs b.showTextOnly)
        ShowJobs x-> (PostsConfig b.postsToFetch b.postsToShow b.sortBy x b.showTextOnly)
        ShowTextOnly x -> (PostsConfig b.postsToFetch b.postsToShow b.sortBy b.showJobs x)

fhelp l acc =
            case l of
                [] ->  acc
                x::xs -> if (x.type_ /= "job") then fhelp xs acc++[x]
                        else fhelp xs acc
fhelp2 l acc =
            case l of
                [] -> acc
                x::xs -> if (x.url /= Nothing) then fhelp2 xs acc++[x]
                        else fhelp2 xs acc

fjobs a b =
    if a.showJobs == False then
        fhelp b []
    else 
        b

furl a b =
    if a.showTextOnly == False then
        fhelp2 b []
    else
        b

limit l n =
    let 
        limhelp lis b acc =
            case lis of
                [] -> acc
                x::xs -> 
                    if b<=n then
                        limhelp xs (b+1) acc++[x]
                    else
                        acc
    in
        limhelp l 1 []




filterPosts : PostsConfig -> List Post -> List Post
filterPosts a b =
     List.sortWith (sortToCompareFn a.sortBy) (List.reverse( (limit ((furl a (fjobs a b)) )  a.postsToShow) ))

    


