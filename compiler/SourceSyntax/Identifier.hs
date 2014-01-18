{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SourceSyntax.Identifier where

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as PP

newtype LowIdent = LowIdent { unLow :: String } deriving (Eq,Ord,Show)
instance Pretty LowIdent where
  pretty (LowIdent s) = text s

newtype CapIdent = CapIdent { unCap :: String } deriving (Eq,Ord,Show)
instance Pretty CapIdent where
  pretty (CapIdent s) = text s

newtype Operator = Operator { unOp :: String } deriving (Eq,Ord,Show)
instance Pretty Operator where
  pretty (Operator s) = text s

