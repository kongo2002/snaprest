{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Template
    (
      toJSONFunc
    , fromJSONFunc
    , toVal
    , fromVal
    ) where

import           Control.Monad  ( mapM, fmap, return )
import           Data.Aeson     ( object, (.=), toJSON, Value(..) )
import           Data.Aeson.TH  ( mkParseJSON )
import qualified Data.Bson as B
import           Data.Char      ( isUpper, toLower )
import           Data.Eq        ( (==) )
import           Data.Bool      ( Bool(..) )
import           Data.Function  ( (.), ($), id )
import           Data.List      ( map, (++), zip, concatMap, length )
import           Data.Maybe     ( Maybe(..), catMaybes, maybe )
import qualified Data.Text as T ( pack )
import           Prelude        ( error )
import           Text.Show      ( show )

import           Language.Haskell.TH

sanitizeField str@(x:xs)
    | x == '_'  = xs
    | isUpper x = toLower x : xs
    | True      = str

field conv = stringL . conv . nameBase

fieldExpr conv = litE . field conv

useType name func = do
    info <- reify name
    case info of
      TyConI decl ->
        case decl of
          DataD _ _ tvbs ctors _ -> func tvbs ctors
          other -> error $ "Utils.Template: Unsupported type: " ++ show other
      _ -> error "Utils.Template: No type name found"

toJSONFunc name =
    useType name (\_ ctors -> ctorLambda buildJsonArgs sanitizeField ctors)

fromJSONFunc = mkParseJSON id

ctorLambda exprFunc nameConv ctors = do
    value <- newName "value"
    lam1E (varP value) $
        caseE (varE value)
              [ exprFunc nameConv ctor
              | ctor <- ctors
              ]

toVal name =
    useType name (\_ ctors -> ctorLambda buildToValArgs sanitizeField ctors)

fromVal name =
    useType name (\_ ctors -> do
        value <- newName "value"
        lam1E (varP value) $
            caseE (varE value)
                  ([buildFromValArgs sanitizeField ctor | ctor <- ctors] ++ [wildcardMatch]))

wildcardMatch = do
    match wildP (normalB [e|Nothing|]) []

stringValue nameConv ctor =
    [e|B.String|] `appE` ([e|T.pack|] `appE` (fieldExpr nameConv $ ctor))

buildToValArgs nameConv (NormalC ctor []) = do
    match (conP ctor [])
          (normalB $ stringValue nameConv ctor)
          []

buildToValArgs _ _ = error "Utils.Template: specified constructor type is not supported yet"

buildFromValArgs nameConv (NormalC ctor []) = do
    match (conP strName [litP $ field nameConv ctor])
          (normalB $ [e|Just|] `appE` conE ctor)
          []
    where
      strName = mkName "String"

buildFromValArgs _ _ = error "Utils.Template: specified constructor type is not supported yet"

buildJsonArgs nameConv (RecC ctorName types) = do
    -- get argument names
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip types [1..]]
    -- build field expressions
    let fields = [ fieldToJsonExpr nameConv arg field ty
                 | (arg, (field, _, ty)) <- zip args types
                 ]
    -- match expression
    match (conP ctorName $ map varP args)
          -- object $ catMaybes [ ... ]
          (normalB $ [e|object|] `appE` ([e|catMaybes|] `appE` listE fields))
          []

buildJsonArgs nameConv (NormalC ctor []) = do
    match (conP ctor [])
          (normalB $ [e|toJSON|] `appE` ([e|T.pack|] `appE` (fieldExpr nameConv $ ctor)))
          []

buildJsonArgs nameConv (NormalC ctor types) = do
    let len = length types
    args <- mapM newName ["arg" ++ show n | n <- [1..len]]
    field <- case [ [e|toJSON|] `appE` varE arg | arg <- args ] of
                [expr] -> return expr
                exprs  -> error "Utils.Template: multiple constructor arguments are not supported yet"
    match (conP ctor $ map varP args)
          (normalB field)
          []

fieldToJsonExpr nameConv arg fname ty =
    case ty of
        AppT ListT _            -> listExpr
        AppT (ConT maybeName) _ -> maybeExpr
        someType                -> defExpr
    where
      maybeName = mkName "Data.Maybe.Maybe"

      -- special handling of Maybe types
      maybeExpr = [e|maybe|] `appE` [e|Nothing|] `appE`
        -- maybe Nothing (\_ -> Just ((T.pack "...") .= <fname>)) <fname>
        lam1E wildP
          ([e|Just|] `appE`
            (infixApp ([e|T.pack|] `appE` fieldExpr nameConv fname)
                      [e|(.=)|]
                      (varE arg))) `appE` (varE arg)

      -- special handling for List types
      listExpr = caseE (varE arg) [
          -- case <fname> of { [] -> Nothing; _ -> defExpr }
          match (listP [])
                (normalB [e|Nothing|])
                [],
          match wildP
                (normalB defExpr)
                []
        ]

      -- default field handling
      defExpr =
        -- Just ((T.pack "...") .= <fname>)
        [e|Just|] `appE`
            (infixApp ([e|T.pack|] `appE` fieldExpr nameConv fname)
                      [e|(.=)|]
                      (varE arg))
