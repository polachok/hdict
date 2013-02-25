{-# LANGUAGE TemplateHaskell #-}
module StarDict.Format.TH where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Control.Monad
import Data.Char

import Control.Applicative
import Data.Attoparsec.Text

buildIFOParser t = do
    TyConI (DataD _ typeName _ constructors _)  <-  reify t

    let gen comb pat = [|(string $(pat) *> char '=' *> $(comb) <* (endOfLine <|> endOfInput))|]

    let buildBody = foldl1 (\a1 a2 -> [|$(a1) <|> $(a2)|])

    let genParser c@(NormalC name fields) = do
        let consName = litE.stringL $ map toLower $ nameBase name
            parserName = mkName $ "parse" ++ nameBase name
            warn x = report False ("Parser for "++nameBase name++" not built") >>
                     gen (global 'empty) x
            parserE = case fields of
                      ((_,ConT ty):[]) ->
                           case nameBase ty of
                                "Integer" -> gen (global 'decimal)
                                "Text" -> gen (appE (global 'takeTill) (global 'isEndOfLine))
                                _ -> warn 
                      _ -> warn
        [ValD _ body dec] <- [d|parse = $(global 'fmap) $(conE name) $(parserE consName)|]
        return $ ValD (VarP parserName) body dec

    ps <- mapM genParser constructors
    body <- buildBody $ map (\(ValD (VarP name) _ _) -> varE name) ps
    return [ValD (VarP (mkName "parseIFO")) (NormalB body) ps]
