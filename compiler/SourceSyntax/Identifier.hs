{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Should definitely hide the constructors, and maybe also the unwrappers
module SourceSyntax.Identifier where

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as PP

newtype LowIdent = LowIdent { unLow :: String } deriving (Eq,Ord,Show)
instance Pretty LowIdent where
  pretty (LowIdent s) = text s

-- Should hide this and put everything that uses it in here
lowmap :: (String -> String) -> LowIdent -> LowIdent
lowmap f (LowIdent s) = LowIdent $ f s

newtype CapIdent = CapIdent { unCap :: String } deriving (Eq,Ord,Show)
instance Pretty CapIdent where
  pretty (CapIdent s) = text s

capmap :: (String -> String) -> CapIdent -> CapIdent
capmap f (CapIdent s) = CapIdent $ f s

newtype Operator = Operator { unOp :: String } deriving (Eq,Ord,Show)
instance Pretty Operator where
  pretty (Operator s) = text s

