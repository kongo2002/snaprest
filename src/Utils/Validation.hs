module Utils.Validation
    ( ensure
    ) where

------------------------------------------------------------------------------
-- | Small function to be used for validation
ensure :: String -> Bool -> Bool -> Either String Bool
ensure _ True True  = Right True
ensure errorMsg _ _ = Left errorMsg
