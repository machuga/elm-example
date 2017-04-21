module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_, value, style, href)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias GitHubLabel =
    { title : String
    , color : String
    }


type alias GitHubIssue =
    { title : String
    , body : String
    , url : String
    , comments : Int
    , labels : List GitHubLabel
    }


type alias Model =
    { repo : String
    , alert : Maybe String
    , issues : List GitHubIssue
    }


type Msg
    = SetRepo String
    | FetchIssues


initialModel : Model
initialModel =
    { repo = ""
    , alert = Nothing
    , issues = []
    }


testIssue : GitHubIssue
testIssue =
    { title = "It Doesn't work"
    , body = "I try to do the thing and it doesn't do the thing."
    , comments = 3
    , url = "https://github.com/elm-lang/elm/issues/123"
    , labels =
        [ { title = "notenoughinfo"
          , color = "fc2929"
          }
        ]
    }



-- Update


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SetRepo repo ->
            ( { model | repo = repo, alert = Nothing }, Cmd.none )

        FetchIssues ->
            if model.repo == "elm-lang/elm" then
                ( { model | issues = [ testIssue ] }, Cmd.none )
            else
                ( { model
                    | issues = []
                    , alert = Just "Try another repo"
                  }
                , Cmd.none
                )



-- View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ alertView model.alert
        , inputView model.repo
        , issuesView model.issues
        , debugView model
        ]


alertView : Maybe String -> Html Msg
alertView alert =
    case alert of
        Just msg ->
            div [ class "alert alert-danger" ]
                [ text msg ]

        Nothing ->
            text ""


inputView : String -> Html Msg
inputView repoName =
    form [ class "form", onSubmit FetchIssues ]
        [ div [ class "form-group" ]
            [ div [ class "row" ]
                [ div [ class "col-md-8" ]
                    [ input
                        [ type_ "text"
                        , onInput SetRepo
                        , class "input-lg form-control"
                        , placeholder "Enter a GitHub Repo (Ex: elm-lang/elm)"
                        , value repoName
                        ]
                        []
                    ]
                , div [ class "col-md-4" ]
                    [ input
                        [ type_ "submit"
                        , onClick FetchIssues
                        , class "btn btn-primary btn-lg"
                        , value "Load Issues"
                        ]
                        []
                    ]
                ]
            ]
        ]


issuesView : List GitHubIssue -> Html msg
issuesView issues =
    div [ class "row" ] (List.map issueView issues)


issueView : GitHubIssue -> Html msg
issueView issue =
    div [ class "col-md-4" ]
        [ article [ class "issue panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ]
                    [ a [ href issue.url ]
                        [ text issue.title ]
                    ]
                ]
            , div [ class "panel-body" ]
                [ p []
                    [ text issue.body ]
                ]
            , div [ class "panel-footer" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6" ]
                        [ div [ class "issue--labels" ]
                            (List.map labelView issue.labels)
                        ]
                    , div [ class "col-md-6" ]
                        [ div [ class "pull-right" ]
                            [ span [ class "badge" ]
                                [ text (toString issue.comments) ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


labelView : GitHubLabel -> Html msg
labelView label =
    span [ class "label", style [ ( "background-color", "#" ++ label.color ) ] ]
        [ text label.title ]


debugView : Model -> Html msg
debugView model =
    pre [] [ text (toString model) ]
