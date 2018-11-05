import Browser
import Html exposing (..)
import Http exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Debug exposing (..)
import RemoteData exposing (..)
import Json.Decode as Decode exposing (Decoder, int, string, float, bool)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)

-- MAIN

main =
    Browser.element
      { init = init
      , view = view
      , update = update
      , subscriptions = (\x -> Sub.none) 
      }
  
-- MODEL


type alias Model =
  { query : String
  , results : WebData (List User)
  , selected : Maybe User
  }

type alias User =
    { login : String
    , id : Int
    , node_id : String
    , avatar_url : String
    , gravatar_id : String
    , url : String
    , html_url : String
    , followers_url : String
    , following_url : String
    , gists_url : String
    , starred_url : String
    , subscriptions_url : String
    , organizations_url : String
    , repos_url : String
    , events_url : String
    , received_events_url : String
    , userType : String
    , site_admin : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
  (Model "" Loading Nothing, fetchUsers)



-- UPDATE


type Msg
  = Search String
  | Click User
  | OnFetchUsers (WebData (List User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Search query ->
      ({ model | query = query }, Cmd.none)

    Click selected ->
      ({ model | selected = Just selected }, Cmd.none)
    
    OnFetchUsers users ->
      ({model | results = users}, Cmd.none)

-- VIEW

innerDivStyle = 
  [ style "border" "1px solid rgba(0, 0, 0, 0.8)"
  , style "display" "inline-grid"
  , style "padding" "5px"
  , style "margin" "5px"
  ]

view : Model -> Html Msg 
view model =
  div [style "display" "grid", style "grid-template-columns" "auto auto auto"]
    [ searchInputWithResults model
    , div innerDivStyle (showSelected model.selected) 
    ]

searchInputWithResults : Model -> Html Msg
searchInputWithResults model =
  div innerDivStyle [
    input [ type_ "text", placeholder "", value model.query, onInput Search ] []
    , ul [] (
      if model.query == ""  
      then displayWebData model.results 
      else
        model.results
        |> RemoteData.map (List.filter (\r -> (String.contains r.login  model.query) ))
        |> displayWebData)
  ]

displayWebData: WebData (List User) -> List (Html Msg)
displayWebData rUsers =
  case rUsers of
    NotAsked ->
      [ ]

    Loading ->
      [ li [] [ text "loading" ] ]

    Success users ->
      usersList users

    Failure err ->
      [ li [] [ text "error" ] ]

showSelected : Maybe User -> List (Html a)
showSelected r =  
  case r of 
    Just selected -> 
      [ li [] [text <| "login :" ++ selected.login ]
      , li [] [text <| "node_id :" ++ selected.node_id ]
      , li [] [text <| "avatar_url :" ++ selected.avatar_url ]
      , li [] [text <| "gravatar_id :" ++ selected.gravatar_id ]
      , li [] [text <| "url :" ++ selected.url ]
      , li [] [text <| "html_url :" ++ selected.html_url ]
      , li [] [text <| "followers_url :" ++ selected.followers_url ]
      , li [] [text <| "following_url :" ++ selected.following_url ]
      , li [] [text <| "gists_url :" ++ selected.gists_url ]
      , li [] [text <| "starred_url :" ++ selected.starred_url ]
      , li [] [text <| "subscriptions_url :" ++ selected.subscriptions_url ]
      , li [] [text <| "organizations_url :" ++ selected.organizations_url ]
      , li [] [text <| "repos_url :" ++ selected.repos_url ]
      , li [] [text <| "events_url :" ++ selected.events_url ]
      , li [] [text <| "received_events_url :" ++ selected.received_events_url ]
      , li [] [text <| "userType :" ++ selected.userType ]
      ]
    Nothing -> 
      []

usersList : List User -> List (Html Msg)
usersList results = 
  List.map (\r -> li [ onClick <| Click <| r ] [ text r.login ]) results



fetchUsers : Cmd Msg
fetchUsers =
    Http.get "https://api.github.com/users?since=135" decodeUsers
        |> RemoteData.sendRequest
        |> Cmd.map OnFetchUsers

decodeUsers : Decoder (List User)
decodeUsers =
  Decode.list decodeUser


decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> required "login" (string)
        |> required "id" (int)
        |> required "node_id" (string)
        |> required "avatar_url" (string)
        |> required "gravatar_id" (string)
        |> required "url" (string)
        |> required "html_url" (string)
        |> required "followers_url" (string)
        |> required "following_url" (string)
        |> required "gists_url" (string)
        |> required "starred_url" (string)
        |> required "subscriptions_url" (string)
        |> required "organizations_url" (string)
        |> required "repos_url" (string)
        |> required "events_url" (string)
        |> required "received_events_url" (string)
        |> required "type" (string)
        |> required "site_admin" (bool)
