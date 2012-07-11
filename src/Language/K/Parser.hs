-----------------------------------------------------------------------------
-- |
-- Module      :  Language.K.Parser
-- Copyright   :  (c) David Lazar, 2012
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Functions (parsers) which translate Maude terms to K terms.
--
-- TODO: this module is unstable and incomplete.
-----------------------------------------------------------------------------

module Language.K.Parser where

import Language.K.Syntax
import Language.Maude.Syntax

anyTerm :: Term -> AnyTerm
anyTerm k@(Term "K" _ _) = KTerm $ kItem k
anyTerm k@(Term "KItem" _ _) = KTerm $ kItem k
anyTerm bag@(Term "NeBag" _ _) = BagTerm $ kBag bag
anyTerm item@(Term "BagItem" _ _) = BagTerm $ KBag [bagItem item]
anyTerm list@(Term "NeList" _ _) = ListTerm $ kList list
anyTerm list@(Term "List" _ _) = ListTerm $ kList list
anyTerm item@(Term "ListItem" _ _) = ListTerm $ KList [listItem item]
anyTerm map@(Term "Map" _ _) = MapTerm $ kMap map
anyTerm map@(Term "NeMap" _ _) = MapTerm $ kMap map
anyTerm nat@(Term "#Zero" _ _) = BuiltinTerm $ builtin nat
anyTerm nat@(IterTerm "#NzNat" _ _ _) = BuiltinTerm $ builtin nat
anyTerm term = unknownTerm term

unknownTerm :: Term -> AnyTerm
unknownTerm (Term sort op ts) = UnknownTerm (splitParts op) (map anyTerm ts)

kBag :: Term -> KBag
kBag (Term _ "__" ts) = KBag (map bagItem ts)

bagItem :: Term -> BagItem
bagItem (Term _ "<_>_</_>" [name, content, _]) = CellItem (termOp name) (anyTerm content) 

kMap :: Term -> KMap
kMap (Term _ "." _) = KMap []
kMap (Term _ "__" ts) = KMap (map mapItem ts)

mapItem :: Term -> MapItem
mapItem (Term _ "_|->_" [k1, k2]) = MapItem (kItem k1) (kItem k2)

kList :: Term -> KList
kList (Term _ _ ts) = KList (map listItem ts)

listItem :: Term -> ListItem
listItem (Term _ "ListItem" [k]) = ListItem (kItem k)
listItem term = UserListItem (unknownTerm term)

kItem :: Term -> K
kItem (Term _ "_`(_`)" [label, listk]) = KApp (kLabel label) (listK listk)
kItem (Term _ "_~>_" ks) = Kra (map kItem ks)
kItem (Term _ "." []) = Kra []
kItem (Term _ "HOLE" []) = FreezerHole
kItem (Term _ "var`{K`}`(_`)" [str]) = FreezerVar (read $ termOp str)

listK :: Term -> [K]
listK (Term _ ".List`{K`}" _) = []
listK (Term _ "_`,`,_" ks) = map kItem ks
listK k@(Term _ _ _) = [kItem k] -- special case for singleton

kLabel :: Term -> KLabel
kLabel (Term _ "#_" [b]) = builtin b
kLabel (Term _ "wmap_" [kmap]) = WMap (kMap kmap)
kLabel (Term _ "kList" [str]) = WKList (read $ termOp str)
kLabel (Term _ "freezer" [k]) = Freezer (kItem k)
kLabel (Term _ "var`{K`}`(_`)<-" [str]) = FreezerMap (read $ termOp str)
kLabel (Term _ ('\'' : label) []) = KLabel $ splitParts label
kLabel term = UserKLabel (unknownTerm term)

builtin :: Term -> KLabel
builtin (Term "#Zero" "0" _) = KInt 0
builtin (IterTerm _ "sNat_" _ n) = KInt n
builtin (Term "#Id" "#id_" [id]) = KId $ read (termOp id)
builtin (Term "#String" str []) = KString (read str)
builtin term = UserKLabel (unknownTerm term) -- TODO should include children

splitParts :: String -> [SyntaxPart]
splitParts s = splitParts' (filter (/= '`') s)

splitParts' :: String -> [SyntaxPart]
splitParts' [] = []
splitParts' ('_' : r) = Hole : splitParts' r
splitParts' l = Syntax s :  splitParts' rest
    where (s, rest) = span (/= '_') l
