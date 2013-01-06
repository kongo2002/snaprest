{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Template
    (
      recordToJSON
    ) where

import           Control.Monad ( mapM, fmap )
import           Data.Aeson
import           Data.Function ( (.), ($), id )
import           Data.List ( map, (++), zip, concatMap )
import           Data.Maybe ( Maybe(..), catMaybes, maybe )
import qualified Data.Text as T ( pack )
import           Prelude ( error )
import           Text.Show ( show )

import           Language.Haskell.TH

sanitizeField ('_':xs) = xs
sanitizeField xs       = xs

fieldExpr conv = litE . stringL . conv . nameBase

useType name func = do
    info <- reify name
    case info of
      TyConI decl ->
        case decl of
          DataD _ _ tvbs ctors _ -> func tvbs ctors
          other -> error $ "Utils.Template: Unsupported type: " ++ show other
      _ -> error "Utils.Template: No type name found"

recordToJSON name =
    useType name (\_ ctors -> recordLambda sanitizeField ctors)

recordLambda nameConv [ctor] = do
    value <- newName "value"
    lam1E (varP value) $
        caseE (varE value)
              [ buildRecordArgs nameConv ctor ]
recordLambda _ ctors = error "Utils.Template: multiple constructor types not supported yet"

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

fieldToJsonExpr nameConv arg fname ty =
    case ty of
        AppT (ConT maybeName) _ -> maybeExpr
        someType                -> defExpr
    where
      maybeName = mkName "Data.Maybe.Maybe"

      maybeExpr = [e|maybe|] `appE` [e|Nothing|] `appE`
        -- maybe Nothing (\_ -> Just ((T.pack "...") .= <fname>)) <fname>
        lam1E wildP
          ([e|Just|] `appE`
            (infixApp ([e|T.pack|] `appE` fieldExpr nameConv fname)
                      [e|(.=)|]
                      (varE arg))) `appE` (varE arg)

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

