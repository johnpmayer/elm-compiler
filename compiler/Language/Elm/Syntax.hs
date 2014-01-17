
{-# OPTIONS_GHC -W #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Elm.Syntax 
  ( P.Pretty(P.pretty)
  , Module
  ) where

import qualified SourceSyntax.PrettyPrint as P
import qualified SourceSyntax.Module as M
import qualified SourceSyntax.Declaration as D

newtype Module = Module (M.Module D.Declaration) 
  deriving (P.Pretty)

{- This section moved to SourceSyntax.Module -}

instance P.Pretty def => P.Pretty (M.Module def) where
  pretty = undefined

