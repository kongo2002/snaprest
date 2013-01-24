{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.Template
    ( toJSONFunc
    , fromJSONFunc
    , toVal
    , fromVal
    ) where

import           Control.Monad  ( mapM, return )
import           Data.Aeson     ( object, (.=), toJSON )
import           Data.Aeson.TH  ( mkParseJSON )
import qualified Data.Bson as B
import           Data.Char      ( isUpper, toLower )
import           Data.Eq        ( (==) )
import           Data.Bool      ( Bool(..) )
import           Data.Function  ( (.), ($), id )
import           Data.List      ( map, (++), zip, length )
import           Data.Maybe     ( Maybe(..), catMaybes, maybe )
import qualified Data.Text as T ( pack )
import           Prelude        ( error, Integer, String )
import           Text.Show      ( show )

import           Language.Haskell.TH

------------------------------------------------------------------------------
-- | Remove any leading underscore or uppercase letters
sanitizeField :: String -> String
sanitizeField [] = []
sanitizeField str@(x:xs)
    | x == '_'  = xs
    | isUpper x = toLower x : xs
    | True      = str


fieldL :: (String -> String) -> Name -> Lit
fieldL conv = stringL . conv . nameBase


fieldExpr :: (String -> String) -> Name -> ExpQ
fieldExpr conv = litE . fieldL conv


errF :: String -> a
errF msg = error $ "Utils.Template: " ++ msg

------------------------------------------------------------------------------
-- | Utility function to execute a function based on its contructors by a
-- specified type name
useType :: Name -> ([TyVarBndr] -> [Con] -> Q Exp) -> Q Exp
useType name func = do
    info <- reify name
    case info of
      TyConI decl ->
        case decl of
          DataD _ _ tvbs ctors _ -> func tvbs ctors
          other -> errF $ "Unsupported type: " ++ show other
      _ -> errF "No type name found"


------------------------------------------------------------------------------
-- | Build a `toJSON` instance function for the type with the specified
-- name
toJSONFunc :: Name -> Q Exp
toJSONFunc name =
    useType name (\_ ctors -> ctorLambda buildJsonArgs sanitizeField ctors)


------------------------------------------------------------------------------
-- | Build a `fromJSON` instance function for the type with the specified
-- name
fromJSONFunc :: Name -> Q Exp
fromJSONFunc = mkParseJSON id


------------------------------------------------------------------------------
-- | Build a lambda expression to be used for a constructor function
ctorLambda :: ((String -> String) -> Con -> MatchQ)
           -> (String -> String)
           -> [Con]
           -> ExpQ
ctorLambda exprFunc nameConv ctors = do
    value <- newName "value"
    lam1E (varP value) $
        caseE (varE value)
              [ exprFunc nameConv ctor
              | ctor <- ctors
              ]


------------------------------------------------------------------------------
-- | Inner function to be used for the `toVal` expression
toVal :: Name -> Q Exp
toVal name =
    useType name (\_ ctors -> ctorLambda buildToValArgs sanitizeField ctors)


------------------------------------------------------------------------------
-- | Inner function to be used for the `fromVal` expression
fromVal :: Name -> Q Exp
fromVal name =
    useType name (\_ cs -> do
        value <- newName "value"
        lam1E (varP value) $
            caseE (varE value)
                  ([buildFromValArgs sanitizeField c | c <- cs] ++ [wildcard]))
    where
      wildcard = do
          match wildP (normalB [e|Nothing|]) []


------------------------------------------------------------------------------
-- | Build match expressions for the `toVal` function
buildToValArgs :: (String -> String) -> Con -> MatchQ
buildToValArgs nameConv (NormalC ctor []) = do
    match (conP ctor [])
          (normalB $ stringV)
          []
    where
      stringV =
          [e|B.String|] `appE` ([e|T.pack|] `appE` (fieldExpr nameConv $ ctor))


buildToValArgs _ _ = errF "Specified constructor type is not supported yet"


------------------------------------------------------------------------------
-- | Build match expressions for the `fromVal` function
buildFromValArgs :: (String -> String) -> Con -> MatchQ
buildFromValArgs nameConv (NormalC ctor []) = do
    match (conP strName [litP $ fieldL nameConv ctor])
          (normalB $ [e|Just|] `appE` conE ctor)
          []
    where
      strName = mkName "String"

buildFromValArgs _ _ = errF "Specified constructor type is not supported yet"


------------------------------------------------------------------------------
-- | Build match expressions for the JSON function `toJSON` and `fromJSON`
-- functions
buildJsonArgs :: (String -> String) -> Con -> MatchQ
buildJsonArgs nameConv (RecC ctorName types) = do
    -- get argument names
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip types [1 :: Integer ..]]
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

buildJsonArgs _ (NormalC ctor types) = do
    let len = length types
    args <- mapM newName ["arg" ++ show n | n <- [1..len]]
    field <- case [ [e|toJSON|] `appE` varE arg | arg <- args ] of
                [expr] -> return expr
                _      -> errF "Multiple constructor arguments are not supported yet"
    match (conP ctor $ map varP args)
          (normalB field)
          []

buildJsonArgs _ _ = errF "Constructor type is not supported yet"


------------------------------------------------------------------------------
-- | Convert the given field to an appropriate expression for the JSON
-- function
fieldToJsonExpr :: (String -> String) -> Name -> Name -> Type -> ExpQ
fieldToJsonExpr nameConv arg fname ty =
    case ty of
        AppT ListT _                            -> listExpr
        -- TODO: there has to be something better...
        AppT (ConT m) _ | (show m) == maybeName -> maybeExpr
        _                                       -> defExpr
    where
      maybeName = "Data.Maybe.Maybe"

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
