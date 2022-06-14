port module Main exposing (..)

import Url as Url
import Url.Builder as Url
import Random as Random
import Task as Task
import Time as Time
import Dict exposing (Dict)
import Dict as Dict
import Set as Set
import Set exposing (Set)
import Browser
import Html exposing (Html, button, div, text, datalist, option, input, form, br, a, span, p, select)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes as A
import Parser exposing (..)
import Parser as Parser
import Result.Extra exposing (combineMap)
import Json.Decode as Decode
import Json.Encode as Encode
import FunctionSet exposing (FunctionSet (..), functionSetEncoder, functionSetDecoder, mkFunctionUrl, displayName)

port persistState : Encode.Value -> Cmd msg

type alias InitFlags =
  { initialSeed : Int
  , initialState : Maybe PersistedState
  , initialSet : String
  }

type alias PersistedState =
  { guesses : Dict String (List Function)
  , initialSeed : Int
  , gameNumber : Dict String Int
  , functionSet : FunctionSet
  }

type Msg = Input String
         | Guess
         | NextGame
         | ChangeSet String
         | DisplayChangeSetPicker

type TypeElem
  = Literal String
  | Ident String

type alias Signature = List TypeElem

type alias Function =
  { name : String
  , signature : Signature
  }

type alias GameState =
  { answer : Function
  , guesses : Dict String (List Function)
  , knownIdents : Set String
  , knownChars : Set Char
  , input : String
  , allSets : Dict String (Dict String Signature)
  , initialSeed : Int
  , nextSeed : Random.Seed
  , gameNumber : Dict String Int
  , error : Maybe String
  , functionSet : FunctionSet
  , showFunctionSetPicker : Bool
  }

type Model =
    FunctionsError
  | Loaded GameState
  

main =
         Browser.element
           { init = init
           , update = update
           , view = view
           , subscriptions = \_ -> Sub.none
           }

init : Encode.Value -> (Model, Cmd Msg)
init initFlags =
  case Decode.decodeValue flagsDecoder initFlags of
    Err e -> (FunctionsError, Cmd.none)
    Ok ({initialSeed, initialState, initialSet}) ->
        ( case results of
            Err _ -> FunctionsError
            Ok allSets -> computeState allSets initialSeed initialState (FunctionSet.fromKey initialSet)
        , Cmd.none
        )

flagsDecoder : Decode.Decoder InitFlags
flagsDecoder =
  Decode.map3 InitFlags
    (Decode.field "initialSeed" Decode.int)
    (Decode.maybe (Decode.field "initialState" (persistedStateDecoder)))
    (Decode.field "initialSet" Decode.string)

persistedStateDecoder : Decode.Decoder PersistedState
persistedStateDecoder =
  let
      fset = Decode.oneOf
               [ Decode.field "functionSet" functionSetDecoder
               , Decode.succeed HaskellPrelude
               ]
      gn = Decode.oneOf
               [ Decode.dict Decode.int
               , Decode.int
                   |> Decode.map (Dict.singleton (FunctionSet.asKey HaskellPrelude))
               ]
      gs = Decode.oneOf
               [ Decode.dict (Decode.list functionDecoder)
               , Decode.list functionDecoder
                   |> Decode.map (Dict.singleton (FunctionSet.asKey HaskellPrelude))
               ]
   in
      Decode.map4 PersistedState
        (Decode.field "guesses" gs)
        (Decode.field "initialSeed" Decode.int)
        (Decode.field "gameNumber" gn)
        fset

persistedStateEncoder : PersistedState -> Encode.Value
persistedStateEncoder { guesses, initialSeed, gameNumber, functionSet } =
  Encode.object [ ("guesses", Encode.dict identity (Encode.list functionEncoder) guesses)
                , ("initialSeed", Encode.int initialSeed)
                , ("gameNumber", Encode.dict identity Encode.int gameNumber)
                , ("functionSet", functionSetEncoder functionSet)
                ]

functionDecoder : Decode.Decoder Function
functionDecoder =
  Decode.map2 Function
    (Decode.field "name" Decode.string)
    (Decode.field "signature" (Decode.list typeElemDecoder))

functionEncoder {name, signature} =
  Encode.object [ ("name", Encode.string name)
                , ("signature", Encode.list typeElemEncoder signature)
                ]

typeElemDecoder : Decode.Decoder TypeElem
typeElemDecoder =
  let
      raw = Decode.map2 (\kind value -> (kind, value))
              (Decode.field "kind" Decode.string)
              (Decode.field "value" Decode.string)
   in
      raw |> Decode.andThen (\(kind, value) ->
        case kind of
          "lit"   -> Decode.succeed (Literal value)
          "ident" -> Decode.succeed (Ident value)
          _       -> Decode.fail ("Unknown kind " ++ kind ++ " not in (lit,ident)")
      )

typeElemEncoder elem =
  case elem of
    Literal value -> Encode.object [ ("kind", Encode.string "lit")
                                   , ("value", Encode.string value)
                                   ]
    Ident value -> Encode.object [ ("kind", Encode.string "ident")
                                 , ("value", Encode.string value)
                                 ]

computeState : Dict String (Dict String Signature)
            -> Int
            -> Maybe PersistedState
            -> Maybe FunctionSet
            -> Model
computeState allSets initialSeed mState mFs =
  let
      defaultSet = Maybe.withDefault HaskellPrelude mFs
      fromScratch =
        case getAnswer (Random.initialSeed initialSeed) defaultSet allSets of
          (Nothing, _)   -> FunctionsError
          (Just a, nextSeed) -> Loaded { answer = a
                                       , guesses = Dict.empty
                                       , knownIdents = Set.empty
                                       , knownChars = Set.empty
                                       , input = ""
                                       , allSets = allSets
                                       , initialSeed = initialSeed
                                       , nextSeed = nextSeed
                                       , gameNumber = Dict.singleton (FunctionSet.asKey defaultSet)
                                                                     1
                                       , error = Nothing
                                       , functionSet = defaultSet
                                       , showFunctionSetPicker = False
                                       }
   in
      case mState of
        Nothing -> fromScratch
        Just persisted_ ->
          let
              persisted = case mFs of
                            Nothing -> persisted_
                            Just fs -> { persisted_ | functionSet = fs }
           in
              if initialSeed /= persisted.initialSeed
              then fromScratch
              else
                case getNthAnswer (Random.initialSeed initialSeed) persisted.functionSet allSets (activeGameNumber persisted) of
                  (Nothing, _) -> FunctionsError
                  (Just a, nextSeed) ->
                    let
                        (knownIdents, knownChars) = computeKnownIdentsFromScratch a (activeGuesses persisted)
                     in
                        Loaded { answer = a
                               , guesses = persisted.guesses
                               , knownIdents = knownIdents
                               , knownChars = knownChars
                               , input = ""
                               , allSets = allSets
                               , initialSeed = initialSeed
                               , nextSeed = nextSeed
                               , gameNumber = persisted.gameNumber
                               , error = Nothing
                               , functionSet = persisted.functionSet
                               , showFunctionSetPicker = False
                               }

computeKnownIdentsFromScratch : Function -> List Function -> (Set String, Set Char)
computeKnownIdentsFromScratch answer guesses =
  guesses
    |> List.foldl (computeKnownIdents answer) (Set.empty, Set.empty)

computeKnownIdents : Function -> Function -> (Set String, Set Char) -> (Set String, Set Char)
computeKnownIdents answer guess (knownIdents, knownChars) =
  let
      newIdents = Set.union knownIdents (getIdents guess.signature)
      remainingIdents = Set.diff (getIdents answer.signature) knownIdents
      newChars = if not (Set.isEmpty remainingIdents)
                 then Set.empty -- the type is not fully dicovered yet
                 else
                   let
                       guessChars = Set.fromList (String.toList guess.name)
                    in
                       Set.union knownChars guessChars
   in
      (newIdents, newChars)

activeFunctionSet {functionSet,allSets} =
  allSets
    |> Dict.get (FunctionSet.asKey functionSet)
    |> Maybe.withDefault Dict.empty

activeGameNumber {functionSet,gameNumber} =
  gameNumber
    |> Dict.get (FunctionSet.asKey functionSet)
    |> Maybe.withDefault 1

activeGuesses {functionSet,guesses} =
  guesses
    |> Dict.get (FunctionSet.asKey functionSet)
    |> Maybe.withDefault []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (model, msg) of
    (FunctionsError, _) -> (FunctionsError, Cmd.none)
    (Loaded state, Input i) ->
       (Loaded { state | input = i, error = Nothing }, Cmd.none)
    (Loaded state, ChangeSet fss) ->
       case FunctionSet.fromKey fss of
         Just fs ->
           case getNthAnswer
                  (Random.initialSeed state.initialSeed)
                  fs state.allSets
                  (activeGameNumber { state | functionSet = fs}) of
             (Just a, _) -> persist { state | functionSet = fs
                                            , answer = a
                                            , showFunctionSetPicker = False
                                            , guesses = Dict.empty
                                            , knownIdents = Set.empty
                                            , knownChars = Set.empty
                                            }
             (Nothing, _) -> (Loaded state, Cmd.none)
         Nothing -> (Loaded state, Cmd.none)
    (Loaded state, DisplayChangeSetPicker) ->
       persist { state | showFunctionSetPicker = True }
    (Loaded state, Guess) ->
       if List.member state.input (List.map .name (activeGuesses state))
       then (Loaded { state | error = Just "This function has been tried already"
                            }, Cmd.none)
       else
         case Dict.get state.input (activeFunctionSet state) of
           Nothing -> (Loaded { state | error = Just ("This function is not part of " ++ displayName state.functionSet)
                                      }, Cmd.none)
           Just signature ->
             let
                 g = { name = state.input, signature = signature }
                 (newIdents, newChars) = computeKnownIdents state.answer g (state.knownIdents
                                                                        ,state.knownChars)
              in
                 persist { state |
                           guesses = state.guesses
                                       |> Dict.insert
                                            (FunctionSet.asKey state.functionSet)
                                            (activeGuesses state ++ [g]),
                           knownIdents = newIdents,
                           knownChars = newChars,
                           input = ""
                           }
    (Loaded state, NextGame) ->
       case getAnswer (state.nextSeed) state.functionSet state.allSets of
         (Nothing, _)       -> (FunctionsError, Cmd.none)
         (Just a, nextSeed) -> persist { answer = a
                                       , guesses = Dict.empty
                                       , knownIdents = Set.empty
                                       , knownChars = Set.empty
                                       , input = ""
                                       , allSets = state.allSets
                                       , initialSeed = state.initialSeed
                                       , nextSeed = nextSeed
                                       , gameNumber = state.gameNumber
                                                        |> Dict.insert
                                                             (FunctionSet.asKey state.functionSet)
                                                             (1 + activeGameNumber state)
                                       , error = Nothing
                                       , functionSet = state.functionSet
                                       , showFunctionSetPicker = False
                                       }

persist : GameState -> (Model, Cmd Msg)
persist state =
  let
      { guesses, knownIdents, initialSeed, gameNumber } = state
      toPersist = persistedStateEncoder
                    { guesses = guesses
                    , initialSeed = initialSeed
                    , gameNumber = gameNumber
                    , functionSet = state.functionSet
                    }
   in
      (Loaded state, persistState toPersist)

getAnswer : Random.Seed -> FunctionSet -> Dict String (Dict String Signature) -> (Maybe Function, Random.Seed)
getAnswer seed fs allSets =
  let
      allFuncs = Dict.get (FunctionSet.asKey fs) allSets
                   |> Maybe.withDefault Dict.empty
      maxIndex = Dict.size allFuncs - 1
      (randomIndex, nextSeed) = Random.step (Random.int 0 maxIndex) seed
      randomKey = Dict.keys allFuncs
                     |> List.drop randomIndex
                     |> List.head
      randomValue = randomKey
                     |> Maybe.andThen (\k -> (Dict.get k allFuncs)
                                               |> Maybe.map (\s -> { name = k, signature = s }))
   in
      (randomValue, nextSeed)

getNthAnswer : Random.Seed -> FunctionSet -> Dict String (Dict String Signature) -> Int -> (Maybe Function, Random.Seed)
getNthAnswer seed fs allSets gameNumber =
  let
      go currentSeed currentGame =
           if currentGame < 1
           then (Nothing, currentSeed)
           else if currentGame == 1
           then getAnswer currentSeed fs allSets
           else
             let
                 maxIndex = allSets
                              |> Dict.get (FunctionSet.asKey fs)
                              |> Maybe.withDefault Dict.empty
                              |> Dict.size
                              |> (\s -> s - 1)
                 (_, nextSeed) = Random.step (Random.int 0 maxIndex) currentSeed
              in
                 go nextSeed (currentGame - 1)
   in
      go seed gameNumber

seedFromDay : Time.Zone -> Time.Posix -> Random.Seed
seedFromDay here now =
  let
      day = Time.toDay here now
      month = case Time.toMonth here now of
                     Time.Jan -> 1
                     Time.Feb -> 2
                     Time.Mar -> 3
                     Time.Apr -> 4
                     Time.May -> 5
                     Time.Jun -> 6
                     Time.Jul -> 7
                     Time.Aug -> 8
                     Time.Sep -> 9
                     Time.Oct -> 10
                     Time.Nov -> 11
                     Time.Dec -> 12
      year = Time.toYear here now
      seed = Random.initialSeed (day + month + year + 2)
   in seed

getIdents : Signature -> Set String
getIdents elems =
  let
      keepIdent e =
        case e of
          Ident i -> Just i
          _       -> Nothing
   in elems
        |> List.filterMap keepIdent
        |> Set.fromList

garbleName : Set Char -> String -> String
garbleName knownChars original =
  let
      garbleChar c = if Set.member c knownChars
                     then c
                     else '❓'
   in
      original
        |> String.toList
        |> List.map garbleChar
        |> String.fromList

garble : Set String -> Signature -> String
garble knownIdents sig =
  let
      garbleElem e =
        case e of
          Literal s -> s
          Ident x ->
            if Set.member x knownIdents
            then x
            else "🤷"
  in
     sig
       |> List.map garbleElem
       |> String.concat

display : Signature -> String
display sig =
  let
      displayElem e =
        case e of
          Literal s -> s
          Ident x -> x
   in
      sig
        |> List.map displayElem
        |> String.concat



viewGuess answer guess =
  div [A.class "guess"]
    [ div [A.class "function"]
          [ span [] [text (guess.name ++ " ")]
          , span [] [text " :: "]
          , span [] [text (display guess.signature ++ " ")]
          ]
    , div [A.class "result"]
          [ text (if guess.name == answer.name
                  then "🎉"
                  else "❌")
          ]
    ]

view : Model -> Html Msg
view model =
  case model of
    FunctionsError -> div [] [text "error loading prelude functions"]
    Loaded state -> viewGame state

viewGame : GameState -> Html Msg
viewGame state =
  let
      garbled = garble state.knownIdents
                       state.answer.signature
      garbledName = garbleName state.knownChars
                               state.answer.name
      possibilities = activeFunctionSet state
                        |> Dict.keys
                        |> List.map (\name -> option [A.value name] [])
                        |> datalist [A.id "function-names"]
      hasError = case state.error of
                   Just _ -> True
                   Nothing -> False
      errorClass = if hasError
                   then "error display"
                   else "error"
      mkOption fs = option [ A.value (FunctionSet.asKey fs)
                           , A.selected (fs == state.functionSet)
                           ] [text (displayName fs)]
      setDisplay = if state.showFunctionSetPicker
                   then
                     [ text ("Set: ")
                     , select
                        [onInput ChangeSet]
                        (List.map mkOption FunctionSet.availableSets)
                     , text (" | Game #" ++ String.fromInt (activeGameNumber state) ++ " ")
                     ]
                   else
                     [ text ("Set: ")
                     , a [A.href (FunctionSet.url state.functionSet)]
                         [text (displayName state.functionSet)]
                     , button [onClick DisplayChangeSetPicker] [text "⌄"]
                     , text (" | Game #" ++ String.fromInt (activeGameNumber state) ++ " ")
                     ]
      nameInput = form [onSubmit Guess]
        [ span [A.class errorClass] [text (Maybe.withDefault "" state.error)]
        , input [ onInput Input, A.list "function-names"
                , A.value state.input
                , A.placeholder ("a function from " ++ displayName state.functionSet)
                ] []
        , br [] []
        , button [ A.type_ "submit"
                 , A.disabled hasError
                 ] [text "guess"]
        ]
      formFooter =
        p [A.class "game-number"]
          setDisplay
      guesses = activeGuesses state
  in
     if List.head (List.reverse guesses) == Just state.answer
     then
        gameIsWon state
     else if List.length guesses >= 10
     then
        gameIsLost state
     else
        div []
          [ div [A.class "answer"]
                [ span [] [text (garbledName ++ " ")]
                , span [] [text " :: "]
                , span [] [text garbled]
                ]
          , viewGuesses state
          , possibilities
          , nameInput
          , formFooter
          ]

viewGuesses : GameState -> Html Msg
viewGuesses state =
  let
      empty = List.repeat 10 (div [A.class "empty"] [])
      guesses = activeGuesses state
                  |> List.map (viewGuess state.answer)
   in
      List.take 10 (guesses ++ empty)
        |> div [A.class "guesses"]

gameIsOver : Bool -> GameState -> Html Msg
gameIsOver isWon state =
  let
      answer = div [ A.class "answer" ]
                   [ span [] [text (state.answer.name ++ " ")]
                   , span [] [text " :: "]
                   , span [] [text (display state.answer.signature ++ " ")]
                   , a [A.href (mkFunctionUrl state.functionSet state.answer.name)] [text "view definition"]
                   ]
      guesses = viewGuesses state
      shareButton = a [A.href (twitterUrl state), A.class "share-link"]
                      [text "Share on twitter"]
      nextGame = div [A.class "next-game"]
                     [ button [onClick NextGame] [text "Try another one"]
                     ]
   in
      if isWon
      then div [] [answer, guesses, shareButton, nextGame]
      else div [] [answer, guesses, nextGame]

gameIsWon : GameState -> Html Msg
gameIsWon = gameIsOver True

gameIsLost : GameState -> Html Msg
gameIsLost = gameIsOver False

twitterUrl : GameState -> String
twitterUrl state =
  Url.crossOrigin
    "https://twitter.com"
    ["intent", "tweet"]
    [Url.string "text" (twitterMessage state)]

twitterMessage : GameState -> String
twitterMessage state = String.join " "
  [ "I've found today's https://haskle.net"
  , FunctionSet.asKey state.functionSet ++ "#" ++ String.fromInt (activeGameNumber state)
  , "function after"
  , String.fromInt (List.length (activeGuesses state))
  , "guess(es)!"
  ]

func : Parser Function
func = succeed Function
         |= variable { start = \_ -> True
                     , inner = \c -> c /= ' '
                     , reserved = Set.empty
                     }
         |. spaces
         |. symbol "::"
         |. spaces
         |= sigParser
         |. end

sigParser : Parser Signature
sigParser =
  let
      go revItems = oneOf
           [ succeed (\item -> Loop (item :: revItems))
               |= typeElem
           , succeed ()
               |> map (\_ -> Done (List.reverse revItems))
           ]
   in
      loop [] go

typeElem : Parser TypeElem
typeElem = oneOf
  [ Parser.map Literal sigLiteral
  , Parser.map Ident sigIdent
  ]

sigLiteral : Parser String
sigLiteral = getChompedString <|
  succeed ()
    |. chompIf (\c -> not (Char.isAlphaNum c))
    |. chompWhile (\c -> not (Char.isAlphaNum c))

sigIdent : Parser String
sigIdent = getChompedString <|
  succeed ()
    |. chompIf (\c -> Char.isAlphaNum c || c == '\'')
    |. chompWhile (\c -> Char.isAlphaNum c || c == '\'')


parseOneSet : FunctionSet -> Result (List DeadEnd) (Dict String Signature)
parseOneSet fs =
  let
      strings = FunctionSet.getAllFuncs fs
   in
      case combineMap (run func) strings of
        Ok parsedFuncs ->
             parsedFuncs
               |> List.map (\f -> (f.name, f.signature))
               |> Dict.fromList
               |> Ok
        Err x -> Err x




results : Result () (Dict String (Dict String Signature))
results =
  let
      keepSuccess (fs, res) = case res of
        Ok x -> Just (fs, x)
        _    -> Nothing
      abortOnFailures pairs =
        if List.length pairs > 0
        then Ok (Dict.fromList pairs)
        else Err ()
   in
      FunctionSet.availableSets
        |> List.map (\fs -> (FunctionSet.asKey fs, parseOneSet fs))
        |> List.filterMap keepSuccess
        |> abortOnFailures
