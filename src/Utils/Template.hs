{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Template
    (
      toJSONFunc
    , fromJSONFunc
    , getRecordFields
    ) where

import           Control.Monad  ( mapM, fmap, return )
import           Data.Aeson     ( object, (.=), toJSON )
import           Data.Aeson.TH  ( mkParseJSON )
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

fieldExpr conv = litE . stringL . conv . nameBase

useType name func = do
    info <- reify name
    case info of
      TyConI decl ->
        case decl of
          DataD _ _ tvbs ctors _ -> func tvbs ctors
          other -> error $ "Utils.Template: Unsupported type: " ++ show other
      _ -> error "Utils.Template: No type name found"

toJSONFunc name =
    useType name (\_ ctors -> recordLambda sanitizeField ctors)

fromJSONFunc = mkParseJSON id

recordLambda nameConv [ctor] = do
    value <- newName "value"
    lam1E (varP value) $
        caseE (varE value)
              [ buildRecordArgs nameConv ctor ]

recordLambda nameConv ctors = do
    value <- newName "value"
    lam1E (varP value) $
        caseE (varE value)
              [ buildRecordArgs nameConv ctor
              | ctor <- ctors
              ]

buildRecordArgs nameConv (RecC ctorName types) = do
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

buildRecordArgs nameConv (NormalC ctor []) = do
    match (conP ctor [])
          (normalB $ fieldExpr nameConv $ ctor)
          []

buildRecordArgs nameConv (NormalC ctor types) = do
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

--------------------------------------------------------------------------------
-- Testing functions
--------------------------------------------------------------------------------

getRecordFields n = do
    rfs <- fmap getRecordFields' $ reify n
    litE . stringL $ show rfs
    where
      getRecordFields' (TyConI (DataD _ _ _ ctors _)) = concatMap getRF' ctors
      getRecordFields' _ = []

      getRF' (RecC name fields) = [(nameBase name, map getFieldInfo fields)]
      getRF' _ = []

      getFieldInfo (name, _, ty) = (nameBase name, show ty)

