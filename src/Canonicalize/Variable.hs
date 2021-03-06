{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Variable where

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Helpers as Help
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Error.Helpers as Error
import qualified Reporting.Region as R
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Result as Result
import Elm.Utils ((|>))


variable :: R.Region -> Env.Environment -> String -> Result.ResultErr Var.Canonical
variable region env var =
  case toVarName var of
    Right (name, varName)
        | Module.nameIsNative name ->
            Result.var (Var.Canonical (Var.Module name) varName)

    _ ->
        case Set.toList `fmap` Map.lookup var (Env._values env) of
          Just [v] ->
              Result.var v

          Just vs  ->
              preferLocals region env "variable" vs var

          Nothing  ->
              notFound region "variable" (Map.keys (Env._values env)) var


tvar
    :: R.Region
    -> Env.Environment
    -> String
    -> Result.ResultErr
          (Either
              Var.Canonical
              (Var.Canonical, [String], Type.Canonical)
          )
tvar region env var =
  case adts ++ aliases of
    []  -> notFound region "type" (Map.keys (Env._adts env) ++ Map.keys (Env._aliases env)) var
    [v] -> Result.var' extract v
    vs  -> preferLocals' region env extract "type" vs var
  where
    adts =
        map Left (maybe [] Set.toList (Map.lookup var (Env._adts env)))

    aliases =
        map Right (maybe [] Set.toList (Map.lookup var (Env._aliases env)))

    extract value =
        case value of
          Left v -> v
          Right (v,_,_) -> v


pvar
    :: R.Region
    -> Env.Environment
    -> String
    -> Int
    -> Result.ResultErr Var.Canonical
pvar region env var actualArgs =
  case Set.toList `fmap` Map.lookup var (Env._patterns env) of
    Just [value] ->
        foundArgCheck value

    Just values ->
        preferLocals' region env fst "pattern" values var
          `Result.andThen` foundArgCheck

    Nothing ->
        notFound region "pattern" (Map.keys (Env._patterns env)) var
  where
    foundArgCheck (name, expectedArgs) =
        if actualArgs == expectedArgs
          then Result.var name
          else Result.err (A.A region (Error.argMismatch name expectedArgs actualArgs))


-- FOUND

preferLocals
    :: R.Region
    -> Env.Environment
    -> String
    -> [Var.Canonical]
    -> String
    -> Result.ResultErr Var.Canonical
preferLocals region env =
  preferLocals' region env id


preferLocals'
    :: R.Region
    -> Env.Environment
    -> (a -> Var.Canonical)
    -> String
    -> [a]
    -> String
    -> Result.ResultErr a
preferLocals' region env extract kind possibilities var =
    case filter (isLocal . extract) possibilities of
      [] ->
          ambiguous possibilities

      [v] ->
          Result.var' extract v

      locals ->
          ambiguous locals
    where
      isLocal :: Var.Canonical -> Bool
      isLocal (Var.Canonical home _) =
          case home of
            Var.Local -> True
            Var.BuiltIn -> False
            Var.Module name ->
                name == Env._home env

      ambiguous possibleVars =
          Result.err (A.A region (Error.variable kind var Error.Ambiguous vars))
        where
          vars = map (Var.toString . extract) possibleVars


-- NOT FOUND HELPERS

type VarName =
    Either String (Module.Name, String)


toVarName :: String -> VarName
toVarName var =
  case Help.splitDots var of
    [x] -> Left x
    xs -> Right (init xs, last xs)


noQualifier :: VarName -> String
noQualifier name =
  case name of
    Left x -> x
    Right (_, x) -> x


qualifiedToString :: (Module.Name, String) -> String
qualifiedToString (modul, name) =
  Module.nameToString (modul ++ [name])


isOp :: VarName -> Bool
isOp name =
  Help.isOp (noQualifier name)


-- NOT FOUND

notFound :: R.Region -> String -> [String] -> String -> Result.ResultErr a
notFound region kind possibilities var =
  let name =
          toVarName var

      possibleNames =
          map toVarName possibilities

      (problem, suggestions) =
          case name of
            Left _ ->
                exposedProblem name possibleNames

            Right (modul, varName) ->
                qualifiedProblem modul varName (Either.rights possibleNames)
    in
        Result.err (A.A region (Error.variable kind var problem suggestions))


exposedProblem :: VarName -> [VarName] -> (Error.VarProblem, [String])
exposedProblem name possibleNames =
  let (exposed, qualified) =
          possibleNames
            |> filter (\n -> isOp name == isOp n)
            |> Error.nearbyNames noQualifier name
            |> Either.partitionEithers
  in
      ( Error.ExposedUnknown
      , exposed ++ map qualifiedToString qualified
      )


qualifiedProblem
    :: Module.Name
    -> String
    -> [(Module.Name, String)]
    -> (Error.VarProblem, [String])
qualifiedProblem moduleName name allQualified =
  let availableModules =
        Set.fromList (map fst allQualified)

      moduleNameString =
        Module.nameToString moduleName
  in
      case Set.member moduleName availableModules of
        True ->
            ( Error.QualifiedUnknown moduleNameString name
            , allQualified
                |> filter ((==) moduleName . fst)
                |> map snd
                |> Error.nearbyNames id name
            )

        False ->
            ( Error.UnknownQualifier moduleNameString name
            , Set.toList availableModules
                |> map Module.nameToString
                |> Error.nearbyNames id moduleNameString
            )