module Crawl where

import Network.Browser(  browse,
                         setCookies,
                         request)
import Network.HTTP(     rspBody,
                         getRequest)
import Text.HTML.TagSoup(fromAttrib, 
                         isTagOpenName,
                         parseTags,
                         Tag(..))
import Data.List(isPrefixOf,
                 isSuffixOf)
import Control.Arrow((&&&))

-- |Filter prev/next links.
prevNext :: [ Tag String ] -> (Maybe String, Maybe String)
prevNext = locatePrevNext (Nothing, Nothing)
  where
    locatePrevNext res []  = res
    locatePrevNext (p,n) (tag@(TagOpen "a" _): TagText t: rest) | isSuffixOf "Prev" t = locatePrevNext (Just $ fromAttrib "href" tag, n) rest 
                                                                | isPrefixOf "Next" t = locatePrevNext (p, Just $ fromAttrib "href" tag) rest 
    locatePrevNext res (t:ts) = locatePrevNext res ts
  
-- |Filter all href links in a page.
links :: [ Tag String ] -> [ String ]
links = filter (isPrefixOf "paper.jsp").map (fromAttrib "href").filter (isTagOpenName  "a")


-- | Collect all paper links on a page and the next/prev links, if they exist
collectPage :: [ Tag String ] -> ([ String ], (Maybe String,Maybe String))
collectPage = links &&& prevNext

-- |Get first page of query.
firstPage = body "http://tinyurl.com/mczfmz9"

-- |Follow all `Prev` links till beginning of search and collect paper links.
previousPages :: ([ String ], (Maybe String,Maybe String)) -> IO [ String ]
previousPages (uris, (Nothing, _)) = return uris
previousPages (uris, (Just p, _)) = do
  b <- body $ "http://search.arxiv.org:8081/" ++ p
  let (uris', np) = collectPage $ parseTags b
  previousPages (uris ++ uris', np)

-- |Follow all `Next` links till end of search and collect paper links.
nextPages :: ([ String ], (Maybe String,Maybe String)) -> IO [ String ]
nextPages (uris, (_, Nothing)) = return uris
nextPages (uris, (_, Just p)) = do
  b <- body $ "http://search.arxiv.org:8081/" ++ p
  let (uris', np) = collectPage $ parseTags b
  nextPages (uris ++ uris', np)

-- | Get body of request, ignoring all cookies but following redirections.
body uri = browse $ setCookies [] >> (request (getRequest uri)) >>= (return.rspBody.snd)
