{-# LANGUAGE OverloadedStrings #-}

module Utils.Paging where

import qualified Data.ByteString.Char8 as BS

data PagingInfo = PagingInfo
    { piPageSize :: Int
    , piPage :: Int
    } deriving (Show, Eq)


defaultPageSize :: Int
defaultPageSize = 50


------------------------------------------------------------------------------
-- | Build the pagination @Link@ header string
buildLinkHeader :: PagingInfo -> Int -> BS.ByteString -> BS.ByteString
buildLinkHeader pinfo c url =
    BS.intercalate ", " $ filter (not . BS.null) [getNext, getPrev, getLast]
  where
    page  = piPage pinfo
    size  = piPageSize pinfo
    rest | c `mod` size > 0 = 1
         | otherwise        = 0
    lastpage   = c `div` size + rest
    get        = BS.pack . show
    make p rel = BS.concat
        [ "<", url, "?page=", (get p), "&pagesize=", (get size)
        , ">; rel=\"", rel, "\"" ]

    getNext
        | page < lastpage = make (page+1) "next"
        | otherwise       = BS.empty
    getPrev
        | page > 1  = make (page-1) "prev"
        | otherwise = BS.empty
    getLast
        | page < lastpage = make lastpage "last"
        | otherwise       = BS.empty


------------------------------------------------------------------------------
-- | Reduce the given result list based on the specified PagingResult
filterPaging :: PagingInfo -> [a] -> [a]
filterPaging pinfo =
    take count . drop toSkip
  where
    use = max (0 :: Int)
    count = use $ piPageSize pinfo
    toSkip = use $ count * (piPage pinfo)
