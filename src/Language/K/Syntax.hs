{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.K.Syntax
-- Copyright   :  (c) David Lazar, 2012
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Data types describing the abstract syntax of K.
-----------------------------------------------------------------------------

module Language.K.Syntax where

import Data.Data

data AnyTerm
    = KTerm K
    | KLabelTerm KLabel
    | ListTerm KList
    | BagTerm KBag
    | SetTerm KSet
    | MapTerm KMap
    | BuiltinTerm KLabel
    | UnknownTerm [SyntaxPart] [AnyTerm]
    deriving (Eq, Ord, Show, Data, Typeable)

data K
    = KApp KLabel [K]
    | Kra [K]
    | FreezerVar String
    | FreezerHole
    deriving (Eq, Ord, Show, Data, Typeable)

data KLabel
    = KLabel [SyntaxPart]
    | KNat Integer
    | KInt Integer
    | KBool Bool
    | KString String
    | KId String
    | Freezer K
    | FreezerMap String
    | WMap KMap
    | WBag KBag
    | WKList String
    | UserKLabel AnyTerm
    deriving (Eq, Ord, Show, Data, Typeable)

data SyntaxPart
    = Syntax String
    | Hole
    deriving (Eq, Ord, Show, Data, Typeable)

data KList = KList [ListItem]
    deriving (Eq, Ord, Show, Data, Typeable)

data ListItem
    = ListItem K
    | Buffer K
    | IStream Integer
    | OStream Integer
    | UserListItem AnyTerm
    deriving (Eq, Ord, Show, Data, Typeable)

data KBag = KBag [BagItem]
    deriving (Eq, Ord, Show, Data, Typeable)

data BagItem
    = BagItem K
    | CellItem
        { cellItemLabel :: String
        , cellItemContent :: AnyTerm
        }
    deriving (Eq, Ord, Show, Data, Typeable)

data KSet = KSet [K]
    deriving (Eq, Ord, Show, Data, Typeable)

data KMap = KMap [MapItem]
    deriving (Eq, Ord, Show, Data, Typeable)

data MapItem = MapItem K K
    deriving (Eq, Ord, Show, Data, Typeable)
