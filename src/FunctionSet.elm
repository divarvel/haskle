module FunctionSet exposing (..)

import Url as Url
import Url.Builder as Url
import Json.Decode as Decode
import Json.Decode exposing (andThen, succeed, fail)
import Json.Encode as Encode
import FunctionSet.HaskellPrelude exposing (haskellPrelude)

type FunctionSet
  = HaskellPrelude
  | JustTraverse

availableSets : List FunctionSet
availableSets = [ HaskellPrelude, JustTraverse ]

getAllFuncs : FunctionSet -> List String
getAllFuncs fs = case fs of
  HaskellPrelude -> haskellPrelude
  JustTraverse ->
    [ "traverse :: Traversable t => Applicative f => (a -> f b) -> t a -> f (t b)"
    ]

asKey : FunctionSet -> String
asKey fs = case fs of
  HaskellPrelude -> "haskell-prelude"
  JustTraverse -> "just-traverse"

fromKey : String -> Maybe FunctionSet
fromKey s = case s of
  "haskell-prelude" -> Just HaskellPrelude
  "just-traverse" -> Just JustTraverse
  _                 -> Nothing

functionSetEncoder fs =
  asKey fs
    |> Encode.string

functionSetDecoder : Decode.Decoder FunctionSet
functionSetDecoder =
  let
      withString v =
         case fromKey v of
           Just fs -> succeed fs
           Nothing -> fail "unrecognized function set"
   in
      Decode.string
        |> andThen withString

displayName : FunctionSet -> String
displayName fs = case fs of
  HaskellPrelude -> "the haskell prelude"
  JustTraverse -> "just traverse"

url : FunctionSet -> String
url fs = case fs of
  HaskellPrelude -> "https://hackage.haskell.org/package/base/docs/Prelude.html"
  JustTraverse -> "https://hackage.haskell.org/package/base/docs/Prelude.html#v:traverse"

mkFunctionUrl : FunctionSet -> String -> String
mkFunctionUrl fs name = case fs of
  HaskellPrelude ->
    Url.custom
      (Url.CrossOrigin "https://hackage.haskell.org")
      ["package", "base", "docs", "Prelude.html"]
      []
      (Just ("v:" ++ haddockEscape name))
  JustTraverse ->  mkFunctionUrl HaskellPrelude "traverse"

haddockEscape : String -> String
haddockEscape name =
  let
      escapeChar p char =
        if p char
        then String.fromChar char
        else "-" ++ String.fromInt (Char.toCode char) ++ "-"
      isLegal char =
        case char of
          ':' -> True
          '_' -> True
          '.' -> True
          _   -> Char.isAlphaNum char
   in
      case String.toList name of
        (c :: cs) -> String.concat (escapeChar Char.isAlpha c :: List.map (escapeChar isLegal) cs)
        _ -> name
