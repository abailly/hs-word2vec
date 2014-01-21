module Crawl where

import Network.Browser(  browse,
                         setCookies,
                         request)
import Network.HTTP(     rspBody,
                         getRequest,
                         rspBody,
                         mkRequest,
                         RequestMethod(..),
                         simpleHTTP)
import Network.URI(parseURI,URI)
import Data.Maybe(fromJust)
import Text.HTML.TagSoup(fromAttrib, 
                         isTagOpenName,
                         parseTags,
                         Tag(..))
import Data.List(isPrefixOf,
                 isSuffixOf)
import Control.Arrow((&&&))
import Control.Monad(when)
import Text.Regex.TDFA((=~))
import qualified Data.ByteString.Lazy as L
import System.FilePath(splitExtension)
import System.Posix.Files(fileExist)
import System.Process(system)
import System.Exit(ExitCode(..))

import Concurrent

-- |Filter prev/next links.
prevNext :: [ Tag String ] -> (Maybe String, Maybe String)
prevNext = locatePrevNext (Nothing, Nothing)
  where
    locatePrevNext res []  = res
    locatePrevNext (p,n) (tag@(TagOpen "a" _): TagText t: rest) | isSuffixOf "Prev" t = locatePrevNext (Just $ fromAttrib "href" tag, n) rest 
                                                                | isPrefixOf "Next" t = locatePrevNext (p, Just $ fromAttrib "href" tag) rest 
    locatePrevNext res (_:ts) = locatePrevNext res ts
  
-- |Filter all href links in a page.
papers :: [ Tag String ] -> [ String ]
papers = filter (isPrefixOf "paper.jsp").map (fromAttrib "href").filter (isTagOpenName  "a")

-- | Collect all paper links on a page and the next/prev links, if they exist
collectPage :: [ Tag String ] -> ([ String ], (Maybe String,Maybe String))
collectPage = papers &&& prevNext

-- | Generate paper ID from paper link.
--
-- >>> paperId "paper.jsp?r=cs/9605101&qid=13871620873749a_nCnN_-288443966&qs=%22big+data%22+OR+cloud+OR+%22machine+learning%22+OR+%22artificial+intelligence%22+OR+%22distributed+computing%22"
-- "cs/9605101"
paperId :: String -> String
paperId link = case link =~ "paper.jsp.r=([^&]+)&.*" :: (String,String,String,[String]) of
  (_,_,_,x:_) -> x
  _           -> ""  

-- |Get first page of query.
firstPage :: IO String
firstPage = body "http://search.arxiv.org:8081/?query=%22big+data%22+OR+cloud+OR+%22machine+learning%22+OR+%22artificial+intelligence%22+OR+%22distributed+computing%22&qid=13871620873749a_nCnN_-288443966&startat=40"

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
body :: String -> IO String
body uri = browse $ setCookies [] >> (request (getRequest uri)) >>= (return.rspBody.snd)


-- |Collect all paper ids
allPaperIds :: IO [ String ]
allPaperIds = do
  f <- firstPage
  let page = collectPage $ parseTags f
  prev <- previousPages page
  next <- nextPages ([], snd page)
  return $ map paperId $ prev ++ next

-- |Construct an URI to a paper's PDF from an id
pdfURI :: String -> URI
pdfURI docId = fromJust $ parseURI $ "http://arxiv.org/pdf/" ++ docId ++ "v1.pdf"

-- |Download single PDF
--
-- throw an error if fail to download, returns filname otherwise.
downloadPDF :: String     -- ^Id of document to download
            -> IO String  -- ^File where document has been downloaded to
downloadPDF docId =  do
  resp <- simpleHTTP (mkRequest GET $ pdfURI docId)
  let b = rspBody $ (\ (Right r) -> r) resp
  let f = filename docId
  e <- fileExist f
  when (not e) $ L.writeFile f b
  return f
  where
    filename     did = map replaceChars did ++ ".pdf"
    replaceChars '/' = '_'
    replaceChars c   = c

-- |Dowload all PDF of papers
downloadPDFs :: IO [ String ] 
downloadPDFs = do
  ids <- allPaperIds
  runInThreadPool 10 ids downloadPDF

-- | Convert a PDF file to text.
--
-- This assumes `pdftotext` is available in the PATH.
convertToText :: String        -- file path
              -> IO String  -- Converted file
convertToText pdf = do
  let txt = fst (splitExtension pdf) ++ ".txt"
  exit <- system $ "pdftotext " ++ pdf
  case exit of
    ExitSuccess   -> return txt
    ExitFailure _ -> return ""
      
