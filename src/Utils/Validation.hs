module Utils.Validation
    (
      ensure
    ) where

data Validation a b = Failure a | Success b
    deriving (Show, Eq)

instance Functor (Validation a) where
    fmap _ (Failure x) = Failure x
    fmap f (Success x) = Success $ f x

ensure :: String -> Bool -> Bool -> Either String Bool
ensure _ True True  = Right True
ensure errorMsg _ _ = Left errorMsg
