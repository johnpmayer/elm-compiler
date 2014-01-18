{-# OPTIONS_GHC -Wall #-}
module Transform.Definition where

import Control.Applicative ((<$>))
import SourceSyntax.Expression
import qualified Transform.Expression as Expr

combineAnnotations :: [ParseDef] -> Either String [Def]
combineAnnotations = go
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."

      exprCombineAnnotations = Expr.crawlLet combineAnnotations

      go defs =
          case defs of
            TypeAnnotation name tipe : Def lhs expr : rest | matchNameLHS name lhs ->
                do expr' <- exprCombineAnnotations expr
                   let def = Definition lhs expr' (Just tipe)
                   (:) def <$> go rest

            TypeAnnotation name _  : _ -> Left (msg name)

            Def pat expr : rest ->
                do expr' <- exprCombineAnnotations expr
                   let def = Definition pat expr' Nothing
                   (:) def <$> go rest

            [] -> return []
