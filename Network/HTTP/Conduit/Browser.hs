{-# LANGUAGE CPP, ScopedTypeVariables, FlexibleContexts #-}
-- | This module is designed to work similarly to the Network.Browser module in the HTTP package.
-- The idea is that there are two new types defined: 'BrowserState' and 'BrowserAction'. The
-- purpose of this module is to make it easy to describe a browsing session, including navigating
-- to multiple pages, and have things like cookie jar updates work as expected as you browse
-- around.
--
-- BrowserAction is a monad that handles all your browser-related activities. This monad is
-- actually implemented as a specialization of the State monad, over the BrowserState type. The
-- BrowserState type has various bits of information that a web browser keeps, such as a current
-- cookie jar, the number of times to retry a request on failure, HTTP proxy information, etc. In
-- the BrowserAction monad, there is one BrowserState at any given time, and you can modify it by
-- using the convenience functions in this module.
--
-- A special kind of modification of the current browser state is the action of making a HTTP
-- request. This will do the request according to the params in the current BrowserState, as well
-- as modifying the current state with, for example, an updated cookie jar and location.
--
-- To use this module, you would bind together a series of BrowserActions (This simulates the user
-- clicking on links or using a settings dialogue etc.) to describe your browsing session. When
-- you've described your session, you call 'browse' on your top-level BrowserAction to actually
-- convert your actions into the ResourceT IO monad.
--
-- Here is an example program:
--
-- > import qualified Data.ByteString as B
-- > import qualified Data.ByteString.Lazy as LB
-- > import qualified Data.ByteString.UTF8 as UB
-- > import           Data.Conduit
-- > import           Network.HTTP.Conduit
-- > import           Network.HTTP.Conduit.Browser
-- > 
-- > -- The web request to log in to a service
-- > req1 :: IO (Request (ResourceT IO))
-- > req1 = do
-- >   req <- parseUrl "http://www.myurl.com/login.php"
-- >   return $ urlEncodedBody [ (UB.fromString "name", UB.fromString "litherum")
-- >                           , (UB.fromString "pass", UB.fromString "S33kRe7")
-- >                           ] req
-- > 
-- > -- Once authenticated, run this request
-- > req2 :: IO (Request m')
-- > req2 = parseUrl "http://www.myurl.com/main.php"
-- > 
-- > -- Bind two BrowserActions together
-- > action :: Request (ResourceT IO) -> Request (ResourceT IO) -> BrowserAction (Response LB.ByteString)
-- > action r1 r2 = do
-- >   _ <- makeRequestLbs r1
-- >   makeRequestLbs r2
-- > 
-- > main :: IO ()
-- > main = do
-- >   man <- newManager def
-- >   r1 <- req1
-- >   r2 <- req2
-- >   out <- runResourceT $ browse man $ action r1 r2
-- >   putStrLn $ UB.toString $ B.concat $ LB.toChunks $ responseBody out

module Network.HTTP.Conduit.Browser
    (
    -- * Main
      BrowserAction
    , GenericBrowserAction
    , browse
    , parseRelativeUrl
    , makeRequest
    , makeRequestLbs
    -- * Browser state
    , BrowserState
    , defaultState
    , getBrowserState
    , setBrowserState
    , withBrowserState
    -- ** Manager
    , Manager
    , getManager
    , setManager
    -- ** Location
    -- | The last visited url (similar to the location bar in mainstream browsers).
    -- Location is updated on every request.
    --
    -- default: @Nothing@
    , getLocation
    , setLocation
    , withLocation
    -- ** Cookies
    -- *** Cookie jar
    -- | All the cookies!
    , getCookieJar
    , setCookieJar
    , withCookieJar
    -- *** Cookie filter
    -- | Each new Set-Cookie the browser encounters will pass through this filter.
    -- Only cookies that pass the filter (and are already valid) will be allowed into the cookie jar
    --
    -- default: @const $ const $ return True@
    , getCookieFilter
    , setCookieFilter
    , withCookieFilter
    -- ** Proxies
    -- *** HTTP
    -- | An optional proxy to send all requests through
    -- if Nothing uses Request's 'proxy'
    --
    -- default: @Nothing@
    , getCurrentProxy
    , setCurrentProxy
    , withCurrentProxy
    -- *** SOCKS
    -- | An optional SOCKS proxy to send all requests through
    -- if Nothing uses Request's 'socksProxy'
    --
    -- default: @Nothing@
    , getCurrentSocksProxy
    , setCurrentSocksProxy
    , withCurrentSocksProxy
    -- ** Redirects
    -- | The number of redirects to allow.
    -- if Nothing uses Request's 'redirectCount'
    --
    -- default: @Nothing@
    , getMaxRedirects
    , setMaxRedirects
    , withMaxRedirects
    -- ** Retries
    -- | The number of times to retry a failed connection
    --
    -- default: @0@
    , getMaxRetryCount
    , setMaxRetryCount
    , withMaxRetryCount
    -- ** Timeout
    -- | Number of microseconds to wait for a response.
    -- if Nothing uses Request's 'responseTimeout'
    --
    -- default: @Nothing@
    , getTimeout
    , setTimeout
    , withTimeout
    -- ** Authorities
    -- | A user-provided function that provides optional authorities.
    -- This function gets run on all requests before they get sent out.
    -- The output of this function is applied to the request.
    --
    -- default: @const Nothing@
    , getAuthorities
    , setAuthorities
    , withAuthorities
    -- ** Headers
    -- *** Override headers
    -- | Specifies Headers that should be added to 'Request',
    -- these will override Headers already specified in 'requestHeaders'.
    --
    -- > do insertOverrideHeader ("User-Agent", "rat")
    -- >    insertOverrideHeader ("Connection", "keep-alive")
    -- >    makeRequest def{requestHeaders = [("User-Agent", "kitten"), ("Accept", "everything/digestible")]}
    -- > > User-Agent: rat
    -- > > Accept: everything/digestible
    -- > > Connection: keep-alive
    , getOverrideHeaders
    , setOverrideHeaders
    , withOverrideHeaders
    , getOverrideHeader
    , setOverrideHeader
    , insertOverrideHeader
    , deleteOverrideHeader
    , withOverrideHeader
    -- *** User agent
    -- | What string to report our user-agent as.
    -- if Nothing will not send user-agent unless one is specified in 'Request'
    --
    -- > getUserAgent = lookup "User-Agent" overrideHeaders
    -- > setUserAgent a = insertOverrideHeader ("User-Agent", a)
    --
    -- default: @Just \"http-conduit\"@
    , getUserAgent
    , setUserAgent
    , withUserAgent
    -- ** Error handling
    -- | Function to check the status code. Note that this will run after all redirects are performed.
    -- if Nothing uses Request's 'checkStatus'
    --
    -- default: @Nothing@
    , getCheckStatus
    , setCheckStatus
    , withCheckStatus
    )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Control.Monad.State
import Control.Exception
import qualified Control.Exception.Lifted as LE
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.List (sinkNull)
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HT
import Network.Socks5 (SocksConf)
import Network.URI (URI (..), URIAuth (..), parseRelativeReference, relativeTo, uriToString)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.CaseInsensitive (mk)
import Data.ByteString.UTF8 (fromString)
import Data.List (partition)
import qualified Data.String as IsString (fromString)
import Web.Cookie (parseSetCookie)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map

import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (liftResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Failure (Failure)

data BrowserState = BrowserState
  { currentLocation     :: Maybe URI
  , maxRedirects        :: Maybe Int
  , maxRetryCount       :: Int
  , timeout             :: Maybe Int
  , authorities         :: Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)
  , cookieFilter        :: Request (ResourceT IO) -> Cookie -> IO Bool
  , cookieJar           :: CookieJar
  , currentProxy        :: Maybe Proxy
  , currentSocksProxy   :: Maybe SocksConf
  , overrideHeaders     :: Map.Map HT.HeaderName BS.ByteString
  , browserCheckStatus  :: Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException)
  , manager             :: Manager
  } 

defaultState :: Manager -> BrowserState
defaultState m = BrowserState { currentLocation = Nothing
                              , maxRedirects = Nothing
                              , maxRetryCount = 0
                              , timeout = Nothing
                              , authorities = const Nothing
                              , cookieFilter = const $ const $ return True
                              , cookieJar = def
                              , currentProxy = Nothing
                              , currentSocksProxy = Nothing
                              , overrideHeaders = Map.singleton HT.hUserAgent (fromString "http-conduit")
                              , browserCheckStatus = Nothing
                              , manager = m
                              }

type BrowserAction = GenericBrowserAction (ResourceT IO)

type GenericBrowserAction m = StateT BrowserState m

-- | Do the browser action with the given manager
browse :: Monad m => Manager -> GenericBrowserAction m a -> m a
browse m act = evalStateT act (defaultState m)

-- | Convert an URL relative to current Location into a 'Request'
--
-- Will throw 'InvalidUrlException' on parse failures or if your Location is 'Nothing' (e.g. you haven't made any requests before)
parseRelativeUrl :: Failure HttpException m => String -> GenericBrowserAction m (Request m')
parseRelativeUrl url = maybe err (parseUrl . use) . currentLocation =<< get
  where err = throw $ InvalidUrlException url "Invalid URL"
        uri = fromMaybe err $ parseRelativeReference url
        use = flip (uriToString id) "" . fromMaybe err . relativeTo' uri
#if MIN_VERSION_network(2,4,0)
        relativeTo' x = Just . relativeTo x
#else
        relativeTo' = relativeTo
#endif

-- | Make a request, using all the state in the current BrowserState
makeRequest :: (MonadBaseControl IO m, MonadResource m) => Request (ResourceT IO) -> GenericBrowserAction m (Response (ResumableSource (ResourceT IO) BS.ByteString))
makeRequest request = do
  BrowserState
    { maxRetryCount = max_retry_count
    , maxRedirects = max_redirects
    , timeout = time_out
    , currentProxy  = current_proxy
    , currentSocksProxy = current_socks_proxy
    , overrideHeaders = override_headers
    , browserCheckStatus = current_check_status
    } <- get
  retryHelper (applyOverrideHeaders override_headers $
    request { redirectCount = 0
            , proxy = maybe (proxy request) Just current_proxy
            , socksProxy = maybe (socksProxy request) Just current_socks_proxy
            , checkStatus = \ _ _ -> Nothing
            , responseTimeout = maybe (responseTimeout request) Just time_out
            }) max_retry_count
               (fromMaybe (redirectCount request) max_redirects)
               (fromMaybe (checkStatus request) current_check_status)
               Nothing
  where retryHelper request' retry_count max_redirects check_status e
          | retry_count < 0 = case e of
            Just e' -> LE.throwIO e'
            Nothing -> LE.throwIO TooManyRetries
          | otherwise = do
              resp <- LE.catch (if max_redirects==0
                                  then (\(_,a,_) -> a) `fmap` performRequest request'
                                  else runRedirectionChain request' max_redirects [])
                (\ (e'::HttpException) -> retryHelper request' (retry_count - 1) max_redirects check_status $ Just $ toException e')
              case check_status (responseStatus resp) (responseHeaders resp) of
                Nothing -> return resp
                Just e' -> retryHelper request' (retry_count - 1) max_redirects check_status (Just e')
        performRequest request' = do
              s@(BrowserState { manager = manager'
                              , authorities = auths
                              , cookieJar = cookie_jar
                              , cookieFilter = cookie_filter
                              }) <- get
              now <- liftIO getCurrentTime
              let (request'', cookie_jar') = insertCookiesIntoRequest
                                              (applyAuthorities auths request')
                                              (evictExpiredCookies cookie_jar now) now
              res <- liftResourceT $ http request'' manager'
              (cookie_jar'', response) <- liftIO $ updateMyCookieJar res request'' now cookie_jar' cookie_filter
              put $ s { cookieJar = cookie_jar''
                      , currentLocation = Just $ getUri request''
                      }
              return (request'', res, response)
        runRedirectionChain request' redirect_count ress
          | redirect_count == (-1) = LE.throwIO . TooManyRedirects =<< mapM (liftResourceT . lbsResponse) ress
          | otherwise = do
              (request'', res, response) <- performRequest request'
              let code = HT.statusCode (responseStatus response)
              if code >= 300 && code < 400
                then do request''' <- case getRedirectedRequest request'' (responseHeaders response) code of
                            Just a -> return a
                            Nothing -> LE.throwIO . UnparseableRedirect =<< (liftResourceT $ lbsResponse response)
                        -- Canibalised from Network.HTTP.Conduit, should be made visible there.
                        -- Allow the original connection to return to the
                        -- connection pool immediately by flushing the body.
                        -- If the response body is too large, don't flush, but
                        -- instead just close the connection.
                        let maxFlush = 1024
                            readMay bs =
                                case S8.readInt bs of
                                    Just (i, bs') | BS.null bs' -> Just i
                                    _ -> Nothing
                            sink =
                                case lookup (IsString.fromString "content-length") (responseHeaders res) >>= readMay of
                                    Just i | i > maxFlush -> return ()
                                    _ -> CB.isolate maxFlush =$ sinkNull
                        liftResourceT $ responseBody res $$+- sink
                        runRedirectionChain request''' (redirect_count - 1) (res:ress)
                else return res
        applyAuthorities auths request' = case auths request' of
          Just (user, pass) -> applyBasicAuth user pass request'
          Nothing -> request'

-- | Make a request and pack the result as a lazy bytestring.
--
-- Note: Even though this function returns a lazy bytestring, it does not
-- utilize lazy I/O, and therefore the entire response body will live in memory.
-- If you want constant memory usage, you'll need to use the conduit package and
-- 'makeRequest' directly. 
makeRequestLbs :: (MonadBaseControl IO m, MonadResource m) => Request (ResourceT IO) -> GenericBrowserAction m (Response L.ByteString)
makeRequestLbs = liftResourceT . lbsResponse <=< makeRequest

applyOverrideHeaders :: Map.Map HT.HeaderName BS.ByteString -> Request a -> Request a
applyOverrideHeaders ov request' = request' {requestHeaders = x $ requestHeaders request'}
  where x r = Map.toList $ Map.union ov (Map.fromList r)

updateMyCookieJar :: Response a -> Request (ResourceT IO) -> UTCTime -> CookieJar -> (Request (ResourceT IO) -> Cookie -> IO Bool) -> IO (CookieJar, Response a)
updateMyCookieJar response request' now cookie_jar cookie_filter = do
  filtered_cookies <- filterM (cookie_filter request') $ catMaybes $ map (\ sc -> generateCookie sc request' now True) set_cookies
  return (cookieJar' filtered_cookies, response {responseHeaders = other_headers})
  where (set_cookie_headers, other_headers) = partition ((== (mk $ fromString "Set-Cookie")) . fst) $ responseHeaders response
        set_cookie_data = map snd set_cookie_headers
        set_cookies = map parseSetCookie set_cookie_data
        cookieJar' = foldl (\ cj c -> insertCheckedCookie c cj True) cookie_jar

-- | You can save and restore the state at will
getBrowserState    :: Monad m => GenericBrowserAction m BrowserState
getBrowserState = get
setBrowserState    :: Monad m => BrowserState -> GenericBrowserAction m ()
setBrowserState = put
withBrowserState   :: Monad m => BrowserState -> GenericBrowserAction m a -> GenericBrowserAction m a
withBrowserState s a = do
  current <- get
  put s
  out <- a
  put current
  return out

getLocation    :: Monad m => GenericBrowserAction m (Maybe URI)
getLocation    = get >>= \ a -> return $ currentLocation a
setLocation    :: Monad m => Maybe URI -> GenericBrowserAction m ()
setLocation  b = get >>= \ a -> put a {currentLocation = b}
withLocation   :: Monad m => Maybe URI -> GenericBrowserAction m a -> GenericBrowserAction m a
withLocation a b = do
  current <- getLocation
  setLocation a
  out <- b
  setLocation current
  return out

getMaxRedirects    :: Monad m => GenericBrowserAction m (Maybe Int)
getMaxRedirects    = get >>= \ a -> return $ maxRedirects a
setMaxRedirects    :: Monad m => Maybe Int -> GenericBrowserAction m ()
setMaxRedirects  b = get >>= \ a -> put a {maxRedirects = b}
withMaxRedirects   :: Monad m => Maybe Int -> GenericBrowserAction m a -> GenericBrowserAction m a
withMaxRedirects a b = do
  current <- getMaxRedirects
  setMaxRedirects a
  out <- b
  setMaxRedirects current
  return out

getMaxRetryCount   :: Monad m => GenericBrowserAction m Int
getMaxRetryCount   = get >>= \ a -> return $ maxRetryCount a
setMaxRetryCount   :: Monad m => Int -> GenericBrowserAction m ()
setMaxRetryCount b = get >>= \ a -> put a {maxRetryCount = b}
withMaxRetryCount  :: Monad m => Int -> GenericBrowserAction m a -> GenericBrowserAction m a
withMaxRetryCount a b = do
  current <- getMaxRetryCount
  setMaxRetryCount a
  out <- b
  setMaxRetryCount current
  return out

getTimeout         :: Monad m => GenericBrowserAction m (Maybe Int)
getTimeout         = get >>= \ a -> return $ timeout a
setTimeout         :: Monad m => Maybe Int -> GenericBrowserAction m ()
setTimeout       b = get >>= \ a -> put a {timeout = b}
withTimeout        :: Monad m => Maybe Int -> GenericBrowserAction m a -> GenericBrowserAction m a
withTimeout    a b = do
  current <- getTimeout
  setTimeout a
  out <- b
  setTimeout current
  return out

getAuthorities     :: Monad m => GenericBrowserAction m (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString))
getAuthorities     = get >>= \ a -> return $ authorities a
setAuthorities     :: Monad m => (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)) -> GenericBrowserAction m ()
setAuthorities   b = get >>= \ a -> put a {authorities = b}
withAuthorities    :: Monad m => (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)) -> GenericBrowserAction m a -> GenericBrowserAction m a
withAuthorities a b = do
  current <- getAuthorities
  setAuthorities a
  out <- b
  setAuthorities current
  return out

getCookieFilter    :: Monad m => GenericBrowserAction m (Request (ResourceT IO) -> Cookie -> IO Bool)
getCookieFilter    = get >>= \ a -> return $ cookieFilter a
setCookieFilter    :: Monad m => (Request (ResourceT IO) -> Cookie -> IO Bool) -> GenericBrowserAction m ()
setCookieFilter  b = get >>= \ a -> put a {cookieFilter = b}
withCookieFilter   :: Monad m => (Request (ResourceT IO) -> Cookie -> IO Bool) -> GenericBrowserAction m a -> GenericBrowserAction m a
withCookieFilter a b = do
  current <- getCookieFilter
  setCookieFilter a
  out <- b
  setCookieFilter current
  return out

getCookieJar       :: Monad m => GenericBrowserAction m CookieJar
getCookieJar       = get >>= \ a -> return $ cookieJar a
setCookieJar       :: Monad m => CookieJar -> GenericBrowserAction m ()
setCookieJar     b = get >>= \ a -> put a {cookieJar = b}
withCookieJar      :: Monad m => CookieJar -> GenericBrowserAction m a -> GenericBrowserAction m a
withCookieJar a b = do
  current <- getCookieJar
  setCookieJar a
  out <- b
  setCookieJar current
  return out

getCurrentProxy    :: Monad m => GenericBrowserAction m (Maybe Proxy)
getCurrentProxy    = get >>= \ a -> return $ currentProxy a
setCurrentProxy    :: Monad m => Maybe Proxy -> GenericBrowserAction m ()
setCurrentProxy  b = get >>= \ a -> put a {currentProxy = b}
withCurrentProxy   :: Monad m => Maybe Proxy -> GenericBrowserAction m a -> GenericBrowserAction m a
withCurrentProxy a b = do
  current <- getCurrentProxy
  setCurrentProxy a
  out <- b
  setCurrentProxy current
  return out

getCurrentSocksProxy    :: Monad m => GenericBrowserAction m (Maybe SocksConf)
getCurrentSocksProxy    = get >>= \ a -> return $ currentSocksProxy a
setCurrentSocksProxy    :: Monad m => Maybe SocksConf -> GenericBrowserAction m ()
setCurrentSocksProxy  b = get >>= \ a -> put a {currentSocksProxy = b}
withCurrentSocksProxy   :: Monad m => Maybe SocksConf -> GenericBrowserAction m a -> GenericBrowserAction m a
withCurrentSocksProxy a b = do
  current <- getCurrentSocksProxy
  setCurrentSocksProxy a
  out <- b
  setCurrentSocksProxy current
  return out

getOverrideHeaders :: Monad m => GenericBrowserAction m HT.RequestHeaders
getOverrideHeaders = get >>= \ a -> return $ Map.toList $ overrideHeaders a
setOverrideHeaders :: Monad m => HT.RequestHeaders -> GenericBrowserAction m ()
setOverrideHeaders b = do
    current_user_agent <- getUserAgent
    get >>= \ a -> put a {overrideHeaders = Map.fromList b}
    setUserAgent current_user_agent
withOverrideHeaders:: Monad m => HT.RequestHeaders -> GenericBrowserAction m a -> GenericBrowserAction m a
withOverrideHeaders a b = do
  current <- getOverrideHeaders
  setOverrideHeaders a
  out <- b
  setOverrideHeaders current
  return out
getOverrideHeader :: Monad m => HT.HeaderName -> GenericBrowserAction m (Maybe BS.ByteString)
getOverrideHeader b = get >>= \ a -> return $ Map.lookup b (overrideHeaders a)
setOverrideHeader :: Monad m => HT.HeaderName -> Maybe BS.ByteString -> GenericBrowserAction m ()
setOverrideHeader b Nothing = deleteOverrideHeader b
setOverrideHeader b (Just c) = insertOverrideHeader (b, c)
insertOverrideHeader :: Monad m => HT.Header -> GenericBrowserAction m ()
insertOverrideHeader (b, c) = get >>= \ a -> put a {overrideHeaders = Map.insert b c (overrideHeaders a)}
deleteOverrideHeader :: Monad m => HT.HeaderName -> GenericBrowserAction m ()
deleteOverrideHeader b = get >>= \ a -> put a {overrideHeaders = Map.delete b (overrideHeaders a)}
withOverrideHeader :: Monad m => HT.Header -> GenericBrowserAction m a -> GenericBrowserAction m a
withOverrideHeader (a,b) c = do
  current <- getOverrideHeader a
  insertOverrideHeader (a,b)
  out <- c
  setOverrideHeader a current
  return out

getUserAgent       :: Monad m => GenericBrowserAction m (Maybe BS.ByteString)
getUserAgent       = get >>= \ a -> return $ Map.lookup HT.hUserAgent (overrideHeaders a)
setUserAgent       :: Monad m => Maybe BS.ByteString -> GenericBrowserAction m ()
setUserAgent Nothing = deleteOverrideHeader HT.hUserAgent
setUserAgent (Just b) = insertOverrideHeader (HT.hUserAgent, b)
withUserAgent      :: Monad m => Maybe BS.ByteString -> GenericBrowserAction m a -> GenericBrowserAction m a
withUserAgent a b = do
  current <- getUserAgent
  setUserAgent a
  out <- b
  setUserAgent current
  return out

getCheckStatus    :: Monad m => GenericBrowserAction m (Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException))
getCheckStatus    = get >>= \ a -> return $ browserCheckStatus a
setCheckStatus    :: Monad m => Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException) -> GenericBrowserAction m ()
setCheckStatus  b = get >>= \ a -> put a {browserCheckStatus = b}
withCheckStatus   :: Monad m => Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException) -> GenericBrowserAction m a -> GenericBrowserAction m a
withCheckStatus a b = do
  current <- getCheckStatus
  setCheckStatus a
  out <- b
  setCheckStatus current
  return out

getManager         :: Monad m => GenericBrowserAction m Manager
getManager         = get >>= \ a -> return $ manager a
setManager         :: Monad m => Manager -> GenericBrowserAction m ()
setManager       b = get >>= \ a -> put a {manager = b}

-- | Extract a 'URI' from the request.
-- Canibalised from Network.HTTP.Conduit.Request, should be made visible there.
getUri :: Request m' -> URI
getUri req = URI
    { uriScheme = if secure req
                    then "https:"
                    else "http:"
    , uriAuthority = Just URIAuth
        { uriUserInfo = ""
        , uriRegName = S8.unpack $ host req
        , uriPort = ':' : show (port req)
        }
    , uriPath = S8.unpack $ path req
    , uriQuery = S8.unpack $ queryString req
    , uriFragment = ""
    }
