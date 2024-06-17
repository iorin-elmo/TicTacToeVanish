module Main exposing (..)

import Browser
import Html exposing (Html, div, text, br, input, span)
import Html.Events as Ev
import Html.Attributes exposing (id, class, style)
import Svg exposing (Svg)
import Svg.Attributes as SAtr
import Svg.Events exposing (onClick)
import Dict exposing (Dict)

import Debug exposing (log)

fieldSize = 3


type State
  = O | X

next : State -> State
next state =
  case state of
    O -> X
    X -> O

type alias Model =
  { field : Dict (Int, Int) State
  , queue : List (Int, Int)
  , turn : State
  , gameOverCheck : Maybe State
  }

type Msg
  = Pressed (Int, Int)
  | Continue

initModel : Model
initModel =
  { field = Dict.empty
  , queue = []
  , turn = O
  , gameOverCheck = Nothing
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Pressed (i,j) ->
      if List.length model.queue == 6
      then
        case model.queue of
          hd :: tl ->
            let
              newField = Dict.insert (i,j) model.turn model.field |> Dict.remove hd
            in
              { model
              | field = newField
              , queue = tl ++ [(i,j)]
              , turn = next model.turn
              , gameOverCheck = gameCheck newField
              }

          [] -> model

      else
        let
          newField = Dict.insert (i,j) model.turn model.field
        in
          { model
          | field = newField
          , queue = model.queue ++ [(i,j)]
          , turn = next model.turn
          , gameOverCheck = gameCheck newField
          }

    Continue -> initModel


gameCheck : Dict (Int,Int) State -> Maybe State
gameCheck field =
  let
    checkDiag1 =
      List.range 0 (fieldSize-1)
        |> List.map (\i -> (i, fieldSize-i-1))
        |> List.foldl
            (\c bool ->
              case (Dict.get c field, Dict.get (0,fieldSize-1) field) of
                (Just state, Just hstate) ->
                  (state == hstate) && bool
                _ -> False
            ) True

    checkDiag2 =
      List.range 0 (fieldSize-1)
        |> List.map (\i -> (fieldSize-i-1, i))
        |> List.foldl
            (\c bool ->
              case (Dict.get c field, Dict.get (fieldSize-1,0) field) of
                (Just state, Just hstate) ->
                  (state == hstate) && bool
                _ -> False
            ) True

    checkRow n =
      List.range 0 (fieldSize-1)
        |> List.map (\i -> (i,n))
        |> List.foldl
            (\c bool ->
              case (Dict.get c field, Dict.get (0,n) field) of
                (Just state, Just hstate) ->
                  (state == hstate) && bool
                _ -> False
            ) True

    checkCol n =
      List.range 0 (fieldSize-1)
        |> List.map (\i -> (n,i))
        |> List.foldl
            (\c bool ->
              case (Dict.get c field, Dict.get (n,0) field) of
                (Just state, Just hstate) ->
                  (state == hstate) && bool
                _ -> False
            ) True

    checkLoop : (Int -> Bool) -> (Int -> (Int, Int)) -> Maybe State
    checkLoop checkFunc toCord =
      List.range 0 (fieldSize-1)
        |> List.foldl
            (\i state ->
              case state of
                Nothing ->
                  if checkFunc i
                  then Dict.get (toCord i) field
                  else Nothing
                _ -> state
            ) Nothing

  in
    if checkDiag1
    then Dict.get (0,fieldSize-1) field
    else
      if checkDiag2
      then Dict.get (fieldSize-1,0) field
      else
        case checkLoop checkRow (\i -> (0,i)) of
          Nothing ->
            checkLoop checkCol (\i -> (i,0))
          state -> state


view : Model -> Html Msg
view model =
  let
    panelSize = 50
    margin = 5
    mxStr = String.fromInt <| panelSize*fieldSize

    panelMaker : (Int, Int) -> Svg Msg
    panelMaker (i,j) =
      let
        panelAttributes =
          [ SAtr.x <| String.fromInt (i*panelSize)
          , SAtr.y <| String.fromInt (j*panelSize)
          , SAtr.width <| String.fromInt panelSize
          , SAtr.height <| String.fromInt panelSize
          , SAtr.fill "#ffffff"
          , SAtr.stroke "#000000"
          , SAtr.strokeWidth "1"
          ]

      in
        if Dict.get (i,j) model.field == Nothing && model.gameOverCheck == Nothing
        then
          Svg.rect ((onClick <| Pressed (i,j)) :: panelAttributes) []
        else
          Svg.rect panelAttributes []

    oxMaker : (Int, Int) -> List (Svg Msg)
    oxMaker (i,j) =
      case Dict.get (i,j) model.field of
        Just O ->
          [ Svg.circle
              [ SAtr.cx <| String.fromInt (panelSize//2+i*panelSize)
              , SAtr.cy <| String.fromInt (panelSize//2+j*panelSize)
              , SAtr.r <| String.fromInt (panelSize//2-margin)
              , SAtr.fill "#ffffff"
              , SAtr.stroke "#ff0000"
              , SAtr.strokeWidth "4"
              ][]
          ]
        Just X ->
          [ Svg.line
              [ SAtr.x1 <| String.fromInt (i*panelSize + margin)
              , SAtr.y1 <| String.fromInt (j*panelSize + margin)
              , SAtr.x2 <| String.fromInt ((i+1)*panelSize - margin)
              , SAtr.y2 <| String.fromInt ((j+1)*panelSize - margin)
              , SAtr.strokeWidth "4"
              , SAtr.stroke "#0000ff"
              ][]
          , Svg.line
              [ SAtr.x1 <| String.fromInt (i*panelSize + margin)
              , SAtr.y1 <| String.fromInt ((j+1)*panelSize - margin)
              , SAtr.x2 <| String.fromInt ((i+1)*panelSize - margin)
              , SAtr.y2 <| String.fromInt (j*panelSize + margin)
              , SAtr.strokeWidth "4"
              , SAtr.stroke "#0000ff"
              ][]
          ]
        _ -> []

    panelList =
      List.range 0 (fieldSize-1)
        |> List.map (\i -> List.map (\j -> (i,j)) (List.range 0 (fieldSize-1)))
        |> List.concat
        |> List.map panelMaker

    svgList =
      List.range 0 (fieldSize-1)
        |> List.map (\i -> List.map (\j -> (i,j)) (List.range 0 (fieldSize-1)))
        |> List.concat
        |> List.map oxMaker
        |> List.concat

    gameOverString =
      case model.gameOverCheck of
        Just O -> "O won !!"
        Just X -> "X won !!"
        _ -> ""

    continueButton =
      case model.gameOverCheck of
        Just _ -> div [][Html.button [Ev.onClick Continue][text "Continue"]]
        _ -> div [][]

  in
    Html.div []
      [ Svg.svg
          [ SAtr.width mxStr
          , SAtr.height mxStr
          , SAtr.viewBox <| "0 0 " ++ mxStr ++ " " ++ mxStr
          ]
          (panelList++svgList)
      , div [][Html.text gameOverString]
      , continueButton
      ]



main : Program () Model Msg
main =
  Browser.sandbox
    { init = initModel
    , view = view
    , update = update
    }

--utilities

list2pointStr : List (Int, Int) -> String
list2pointStr list =
  case list of
    (x,y) :: tl ->
      String.fromInt x ++ "," ++ String.fromInt y ++ " " ++ list2pointStr tl
    _ -> ""