module Utils.Validation
    (
      ensure
    ) where

ensure :: String -> Bool -> Bool -> Either String Bool
ensure _ True True  = Right True
ensure errorMsg _ _ = Left errorMsg
