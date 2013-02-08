{-# LANGUAGE CPP, ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
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
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Data.ByteString.Lazy as LB
-- > import qualified Data.Text.Encoding as TE
-- > import qualified Data.Text.Lazy.Encoding as TLE
-- > import qualified Data.Text.Lazy.IO as TLIO
-- > import           Data.Conduit
-- > import           Network.HTTP.Conduit
-- > import           Network.HTTP.Conduit.Browser
-- >
-- > -- The web request to log in to a service
-- > req1 :: IO (Request (ResourceT IO))
-- > req1 = do
-- >   req <- parseUrl "http://www.myurl.com/login.php"
-- >   return $ urlEncodedBody [ (TE.encodeUtf8 "name", TE.encodeUtf8 "litherum")
-- >                           , (TE.encodeUtf8 "pass", TE.encodeUtf8 "S33kRe7")
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
-- >   out <- runResourceT $ browse man $ do
-- >     setDefaultHeader "User-Agent" $ Just "A very popular browser"
-- >     action r1 r2
-- >   TLIO.putStrLn $ TLE.decodeUtf8 $ responseBody out

module Network.HTTP.Conduit.Browser
    (
    -- * Main
      BrowserAction
    , GenericBrowserAction
    , browse
    , parseRelativeUrl
    , makeRequest
    , makeRequestLbs
    , downloadFile
    -- * Browser state
    -- | You can save and restore the state at will
    , BrowserState
    , defaultState
    , getBrowserState
    , setBrowserState
    , withBrowserState
    -- ** Manager
    -- | The active manager, managing the connection pool
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
    -- ** Client certificates
    -- | SSL client certificates
    --
    -- default: @Nothing@
    , getClientCertificates
    , setClientCertificates
    , withClientCertificates
    -- ** Headers
    -- *** Default headers
    -- | Specifies Headers that should be added to 'Request',
    -- these will be overriden by any headers specified in 'requestHeaders'.
    --
    -- > do insertDefaultHeader ("User-Agent", "dog")
    -- >    insertDefaultHeader ("Connection", "keep-alive")
    -- >    makeRequest def{requestHeaders = [("User-Agent", "kitten"), ("Accept", "x-animal/mouse")]}
    -- > > User-Agent: kitten
    -- > > Accept: x-animal/mouse
    -- > > Connection: keep-alive
    --
    -- default: @[(\"User-Agent\", \"http-conduit-browser\")]@
    , getDefaultHeaders
    , setDefaultHeaders
    , withDefaultHeaders
    , getDefaultHeader
    , setDefaultHeader
    , insertDefaultHeader
    , deleteDefaultHeader
    , withDefaultHeader
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
    --
    -- default: @[]@
    , getOverrideHeaders
    , setOverrideHeaders
    , withOverrideHeaders
    , getOverrideHeader
    , setOverrideHeader
    , insertOverrideHeader
    , deleteOverrideHeader
    , withOverrideHeader
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

import Network.HTTP.Conduit
import Network.HTTP.Conduit.Internal (httpRedirect, getUri, setUri)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HT
import Network.Socks5 (SocksConf)
import Network.URI (URI (..), URIAuth (..), parseRelativeReference, relativeTo, uriToString)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Web.Cookie (parseSetCookie)
import Data.Certificate.X509 (X509)
import Network.TLS (PrivateKey)

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.List (sinkNull)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import Data.List (partition)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource (liftResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Failure
import qualified Control.Exception.Lifted as LE
import Control.Exception (SomeException, IOException, toException)
import qualified Data.Map as Map

data BrowserState = BrowserState
  { currentLocation     :: Maybe URI
  , maxRedirects        :: Maybe Int
  , maxRetryCount       :: Int
  , timeout             :: Maybe Int
  , authorities         :: Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)
  , browserClientCertificates :: Maybe [(X509, Maybe PrivateKey)]
  , cookieFilter        :: Request (ResourceT IO) -> Cookie -> IO Bool
  , cookieJar           :: CookieJar
  , currentProxy        :: Maybe Proxy
  , currentSocksProxy   :: Maybe SocksConf
  , overrideHeaders     :: Map.Map HT.HeaderName BS.ByteString
  , defaultHeaders      :: Map.Map HT.HeaderName BS.ByteString
  , browserCheckStatus  :: Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException)
  , manager             :: Manager
  }

defaultState :: Manager -> BrowserState
defaultState m = BrowserState { currentLocation = Nothing
                              , maxRedirects = Nothing
                              , maxRetryCount = 0
                              , timeout = Nothing
                              , authorities = const Nothing
                              , browserClientCertificates = Nothing
                              , cookieFilter = const $ const $ return True
                              , cookieJar = def
                              , currentProxy = Nothing
                              , currentSocksProxy = Nothing
                              , overrideHeaders = Map.empty
                              , defaultHeaders = Map.singleton HT.hUserAgent "http-conduit-browser"
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
parseRelativeUrl url =
    maybe err use =<< gets currentLocation
  where err = lift $ failure $ InvalidUrlException url "Invalid URL"
        use loc = maybe err (setUri def) $ do
            uri <- parseRelativeReference url
            relativeTo' uri loc
#if MIN_VERSION_network(2,4,0)
        relativeTo' x = Just . relativeTo x
#else
        relativeTo' = relativeTo
#endif

-- | Make a request, using all the state in the current BrowserState
makeRequest :: (MonadBaseControl IO m, MonadResource m) => Request (ResourceT IO) -> GenericBrowserAction m (Response (ResumableSource (ResourceT IO) BS.ByteString))
makeRequest req = do
  BrowserState
    { maxRetryCount = max_retry_count
    , maxRedirects = max_redirects
    , timeout = time_out
    , currentProxy  = current_proxy
    , currentSocksProxy = current_socks_proxy
    , defaultHeaders = default_headers
    , overrideHeaders = override_headers
    , browserCheckStatus = current_check_status
    , browserClientCertificates = client_certificates
    } <- get
  retryHelper
    (applyOverrideHeaders override_headers $
     applyDefaultHeaders default_headers $
    req { redirectCount = 0
            , proxy = maybe (proxy req) Just current_proxy
            , socksProxy = maybe (socksProxy req) Just current_socks_proxy
            , checkStatus = \ _ _ -> Nothing
            , responseTimeout = maybe (responseTimeout req) Just time_out
            , clientCertificates = fromMaybe (clientCertificates req) client_certificates
            }) max_retry_count
               (fromMaybe (redirectCount req) max_redirects)
               (fromMaybe (checkStatus req) current_check_status)
               Nothing
  where
    snd3 (_, a, _) = a
    retryHelper request' retry_count max_redirects check_status e
      | retry_count < 0 = case e of
        Just e' -> LE.throwIO e'
        Nothing -> LE.throwIO TooManyRetries
      | otherwise = do
          resp <- LE.catches
            (if max_redirects == 0
                then snd3 `fmap` performRequest request'
                else runRedirectionChain request' max_redirects)
            [ LE.Handler $ \(e'::HttpException) -> retryHelper request' (retry_count - 1) max_redirects check_status $ Just $ toException e'
            , LE.Handler $ \(e'::IOException) -> retryHelper request' (retry_count - 1) max_redirects check_status $ Just $ toException e'
            ]
          case check_status (responseStatus resp) (responseHeaders resp) of
            Nothing -> return resp
            Just e' -> retryHelper request' (retry_count - 1) max_redirects check_status (Just e')
    runRedirectionChain request'' redirect_count
      = httpRedirect
          redirect_count
          (\request' -> do
              (request, res, res') <- performRequest request'
              let mreq = getRedirectedRequest request (responseHeaders res') (HT.statusCode $ responseStatus res')
              return (res, mreq))
          liftResourceT
          request''
    performRequest request' = do
        s@(BrowserState { manager = manager'
                        , authorities = auths
                        , cookieJar = cookie_jar
                        , cookieFilter = cookie_filter
                        }) <- get
        now <- liftIO getCurrentTime
        let (request'', cookie_jar') =
                insertCookiesIntoRequest
                    (applyAuthorities auths request')
                    (evictExpiredCookies cookie_jar now) now
        res <- liftResourceT $ http request'' manager'
        now' <- liftIO getCurrentTime
        (cookie_jar'', response) <- liftIO $
                updateMyCookieJar res request'' now' cookie_jar' cookie_filter
        put $ s { cookieJar = cookie_jar''
                , currentLocation = Just $ getUri request''
                }
        return (request'', res, response)

applyAuthorities :: (Request a -> Maybe (BS.ByteString, BS.ByteString)) -> Request a -> Request a
applyAuthorities auths request' =
    case auths request' of
        Just (user, pass) -> applyBasicAuth user pass request'
        Nothing -> request'

applyDefaultHeaders :: Map.Map HT.HeaderName BS.ByteString -> Request a -> Request a
applyDefaultHeaders dv request = request {requestHeaders = x $ requestHeaders request}
  where x r = Map.toList $ Map.union (Map.fromList r) dv

applyOverrideHeaders :: Map.Map HT.HeaderName BS.ByteString -> Request a -> Request a
applyOverrideHeaders ov request = request {requestHeaders = x $ requestHeaders request}
  where x r = Map.toList $ Map.union ov (Map.fromList r)

-- | Make a request and pack the result as a lazy bytestring.
--
-- Note: Even though this function returns a lazy bytestring, it does not
-- utilize lazy I/O, and therefore the entire response body will live in memory.
-- If you want constant memory usage, you'll need to use the conduit package and
-- 'makeRequest' directly.
makeRequestLbs :: (MonadBaseControl IO m, MonadResource m) => Request (ResourceT IO) -> GenericBrowserAction m (Response L.ByteString)
makeRequestLbs = liftResourceT . lbsResponse <=< makeRequest

-- | Make a request and sink the 'responseBody' to a file.
downloadFile :: (MonadResource m, MonadBaseControl IO m) => FilePath -> Request (ResourceT IO) -> GenericBrowserAction m ()
downloadFile file request = do
  res <- makeRequest request
  liftResourceT $ responseBody res $$+- CB.sinkFile file

updateMyCookieJar :: Response a -> Request (ResourceT IO) -> UTCTime -> CookieJar -> (Request (ResourceT IO) -> Cookie -> IO Bool) -> IO (CookieJar, Response a)
updateMyCookieJar response request' now cookie_jar cookie_filter = do
  filtered_cookies <- filterM (cookie_filter request') $ catMaybes $ map (\ sc -> generateCookie sc request' now True) set_cookies
  return (cookieJar' filtered_cookies, response {responseHeaders = other_headers})
  where (set_cookie_headers, other_headers) = partition ((== "Set-Cookie") . fst) $ responseHeaders response
        set_cookie_data = map snd set_cookie_headers
        set_cookies = map parseSetCookie set_cookie_data
        cookieJar' = foldl (\ cj c -> insertCheckedCookie c cj True) cookie_jar

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

getManager         :: Monad m => GenericBrowserAction m Manager
getManager         = get >>= \ a -> return $ manager a
setManager         :: Monad m => Manager -> GenericBrowserAction m ()
setManager       b = get >>= \ a -> put a {manager = b}

#define RET(x) x
#define CONCAT(x,y) RET(x)y
#define GENERIC_FIELD(Name, field, Type)\
    CONCAT(get,Name)      :: Monad m => GenericBrowserAction m (Type)      ;\
    CONCAT(get,Name)      = gets field                                     ;\
    CONCAT(set,Name)      :: Monad m => (Type) -> GenericBrowserAction m ()  ;\
    CONCAT(set,Name)    b = get >>= \ a -> put a {field = b}               ;\
    CONCAT(with,Name)     :: Monad m => (Type) -> GenericBrowserAction m a -> GenericBrowserAction m a   ;\
    CONCAT(with,Name) a b = do     \
      current <- CONCAT(get,Name) ;\
      CONCAT(set,Name) a          ;\
      out <- b                     ;\
      CONCAT(set,Name) current    ;\
      return out                   ;\

GENERIC_FIELD(Location, currentLocation, Maybe URI)

GENERIC_FIELD(MaxRedirects, maxRedirects, Maybe Int)

GENERIC_FIELD(MaxRetryCount, maxRetryCount, Int)

GENERIC_FIELD(Timeout, timeout, Maybe Int)

GENERIC_FIELD(Authorities, authorities, Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString))

GENERIC_FIELD(ClientCertificates, browserClientCertificates, Maybe [(X509, Maybe PrivateKey)])

GENERIC_FIELD(CookieFilter, cookieFilter, Request (ResourceT IO) -> Cookie -> IO Bool)

GENERIC_FIELD(CookieJar, cookieJar, CookieJar)

GENERIC_FIELD(CurrentProxy, currentProxy, Maybe Proxy)

GENERIC_FIELD(CurrentSocksProxy, currentSocksProxy, Maybe SocksConf)

GENERIC_FIELD(CheckStatus, browserCheckStatus, Maybe (HT.Status -> HT.ResponseHeaders -> Maybe SomeException))

#undef GENERIC_FIELD
#undef CONCAT
#undef RET

getDefaultHeaders :: Monad m => GenericBrowserAction m HT.RequestHeaders
getDefaultHeaders = gets $ Map.toList . defaultHeaders

setDefaultHeaders :: Monad m => HT.RequestHeaders -> GenericBrowserAction m ()
setDefaultHeaders b = get >>= \ a -> put a {defaultHeaders = Map.fromList b}

withDefaultHeaders:: Monad m => HT.RequestHeaders -> GenericBrowserAction m a -> GenericBrowserAction m a
withDefaultHeaders a b = do
  current <- getDefaultHeaders
  setDefaultHeaders a
  out <- b
  setDefaultHeaders current
  return out

getDefaultHeader :: Monad m => HT.HeaderName -> GenericBrowserAction m (Maybe BS.ByteString)
getDefaultHeader b = gets $ Map.lookup b . defaultHeaders

setDefaultHeader :: Monad m => HT.HeaderName -> Maybe BS.ByteString -> GenericBrowserAction m ()
setDefaultHeader b Nothing = deleteDefaultHeader b
setDefaultHeader b (Just c) = insertDefaultHeader (b, c)

insertDefaultHeader :: Monad m => HT.Header -> GenericBrowserAction m ()
insertDefaultHeader (b, c) = get >>= \ a -> put a {defaultHeaders = Map.insert b c (defaultHeaders a)}

deleteDefaultHeader :: Monad m => HT.HeaderName -> GenericBrowserAction m ()
deleteDefaultHeader b = get >>= \ a -> put a {defaultHeaders = Map.delete b (defaultHeaders a)}

withDefaultHeader :: Monad m => HT.Header -> GenericBrowserAction m a -> GenericBrowserAction m a
withDefaultHeader (a,b) c = do
  current <- getDefaultHeader a
  insertDefaultHeader (a,b)
  out <- c
  setDefaultHeader a current
  return out

getOverrideHeaders :: Monad m => GenericBrowserAction m HT.RequestHeaders
getOverrideHeaders = gets $ Map.toList . overrideHeaders

setOverrideHeaders :: Monad m => HT.RequestHeaders -> GenericBrowserAction m ()
setOverrideHeaders b = get >>= \ a -> put a {overrideHeaders = Map.fromList b}

withOverrideHeaders:: Monad m => HT.RequestHeaders -> GenericBrowserAction m a -> GenericBrowserAction m a
withOverrideHeaders a b = do
  current <- getOverrideHeaders
  setOverrideHeaders a
  out <- b
  setOverrideHeaders current
  return out

getOverrideHeader :: Monad m => HT.HeaderName -> GenericBrowserAction m (Maybe BS.ByteString)
getOverrideHeader b = gets $ Map.lookup b . overrideHeaders

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
