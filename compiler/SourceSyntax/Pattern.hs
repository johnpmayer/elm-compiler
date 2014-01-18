{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Pattern where

import qualified SourceSyntax.Helpers as Help
import           SourceSyntax.Identifier
import           SourceSyntax.PrettyPrint
import           Text.PrettyPrint as PP
import qualified Data.Set as Set
import           SourceSyntax.Literal as Literal

data Pattern = PVar LowIdent
             | PAnything
             | PLiteral Literal.Literal
             | PNil
             | PCons Pattern Pattern
             | PTuple [Pattern]
             | PData CapIdent [Pattern]
             | PRecord [LowIdent]
             | PAlias LowIdent Pattern
               deriving (Eq, Ord, Show)

cons :: Pattern -> Pattern -> Pattern
cons h t = PCons h t

nil :: Pattern
nil = PNil

list :: [Pattern] -> Pattern
list = foldr cons nil

tuple :: [Pattern] -> Pattern
tuple = PTuple

boundVars :: Pattern -> Set.Set String
boundVars pattern = 
    case pattern of
      PVar x -> Set.singleton . unLow $ x
      PAnything -> Set.empty
      PLiteral _ -> Set.empty
      PNil -> Set.empty
      PCons h t -> Set.unions . map boundVars $ [h,t]
      PTuple es -> Set.unions (map boundVars es)
      PData _ ps -> Set.unions (map boundVars ps)
      PRecord fields -> Set.fromList . map unLow $ fields
      PAlias x p -> Set.insert (unLow x) (boundVars p)

instance Pretty Pattern where
  pretty pattern = 
    case pattern of
      PVar x -> PP.text . unLow $ x
      PAnything -> PP.text "_"
      PLiteral lit -> pretty lit
      PNil -> PP.text "[]"
      PCons h t -> prettyParens h <+> PP.text "::" <+> pretty t
      PTuple es -> PP.parens . commaCat . map prettyParens $ es
      PData name ps -> hsep (pretty name : map prettyParens ps)
      PRecord fs -> PP.braces . commaCat . map pretty $ fs
      PAlias x p -> prettyParens p <+> PP.text "as" <+> pretty x

prettyParens :: Pattern -> Doc
prettyParens subPat = parensIf needsParens (pretty subPat)
  where
    needsParens = 
      case subPat of
        PCons _ _ -> True
        PData _ (_:_) -> True
        PAlias _ _ -> True
        _ -> False

