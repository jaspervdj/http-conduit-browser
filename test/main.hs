{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Test.Hspec
import Test.HUnit
import Control.Applicative
import Control.Monad
import Control.Exception (Exception, toException)
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai hiding (requestBody, requestHeaders)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Data.ByteString.Base64 (encode)
import Data.Typeable (Typeable)
import Control.Concurrent (forkIO, killThread)
import Network.HTTP.Types
import Control.Exception.Lifted (try)
import Data.CaseInsensitive (mk)
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Data.Time.Calendar
import Web.Cookie

-- TODO tests for responseTimeout/Browser.timeout.

data TestException = TestException
    deriving (Show, Typeable)

instance Exception TestException

utf8String :: String -> S.ByteString
utf8String = TE.encodeUtf8 . T.pack

strictToLazy :: S.ByteString -> L.ByteString
strictToLazy = L.fromChunks . replicate 1

lazyToStrict :: L.ByteString -> S.ByteString
lazyToStrict = S.concat . L.toChunks

dummy :: S.ByteString
dummy = "dummy"

user :: S.ByteString
user = "user"

pass :: S.ByteString
pass = "pass"

failure :: L.ByteString
failure = "failure"

success :: L.ByteString
success = "success"

appWithSideEffect :: IORef Bool -> Application
appWithSideEffect ref _ = liftIO $ do
    v <- readIORef ref
    writeIORef ref $ not v
    if v
        then return $ responseLBS status500 [] failure
        else return $ responseLBS status200 [] success

app :: Application
app req =
    case pathInfo req of
        [] -> return $ responseLBS status200 [] "homepage"
        ["cookies"] -> return $ responseLBS status200 [tastyCookie] "cookies"
        ["print-cookies"] -> return $ responseLBS status200 [] $ getHeader "Cookie"
        ["useragent"] -> return $ responseLBS status200 [] $ getHeader "User-Agent"
        ["accept"] -> return $ responseLBS status200 [] $ getHeader "Accept"
        ["authorities"] -> return $ responseLBS status200 [] $ getHeader "Authorization"
        ["redir1"] -> return $ responseLBS temporaryRedirect307 [redir2] L.empty
        ["redir2"] -> return $ responseLBS temporaryRedirect307 [redir3] L.empty
        ["redir3"] -> return $ responseLBS status200 [] $ strictToLazy dummy
        ["cookie_redir2"] -> return $ responseLBS status303 [("Set-Cookie", "baka=baka;"), (hLocation, "/checkcookie")] ""
        ["checkcookie"] -> return $
            if "flavor=chocolate-chip;baka=baka" == getHeader hCookie
                then responseLBS status200 [] "nom-nom-nom"
                else responseLBS status200 [] $ getHeader "Cookie"
        _ -> return $ responseLBS status404 [] "not found"

    where tastyCookie = (mk (utf8String "Set-Cookie"), utf8String "flavor=chocolate-chip;")
          getHeader s = strictToLazy $ case lookup s $ Network.Wai.requestHeaders req of
                            Just a -> a
                            Nothing -> S.empty
          redir2 = (mk (utf8String "Location"), utf8String "/redir2")
          redir3 = (mk (utf8String "Location"), utf8String "/redir3")

main :: IO ()
main = do
    ref <- newIORef True
    hspec $ do
        describe "browser" $ do
            it "cookie jar works" $ do
                tid <- forkIO $ run 3011 app
                request1 <- parseUrl "http://127.0.0.1:3011/cookies"
                request2 <- parseUrl "http://127.0.0.1:3011/print-cookies"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        _ <- makeRequestLbs request1
                        makeRequestLbs request2
                killThread tid
                if (lazyToStrict $ responseBody elbs) /= utf8String "flavor=chocolate-chip"
                     then error "Should have gotten the cookie back!"
                     else return ()
            it "cookie filter can deny cookies" $ do
                tid <- forkIO $ run 3011 app
                request1 <- parseUrl "http://127.0.0.1:3011/cookies"
                request2 <- parseUrl "http://127.0.0.1:3011/print-cookies"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setCookieFilter $ const $ const $ return False
                        _ <- makeRequestLbs request1
                        makeRequestLbs request2
                killThread tid
                if (lazyToStrict $ responseBody elbs) /= S.empty
                     then error "Shouldn't have gotten the cookie back!"
                     else return ()
            it "user-defined cookies survive redirects" $ do
                tid <- forkIO $ run 3019 app
                req <- parseUrl "http://127.0.0.1:3019/cookie_redir2"
                let setCookie = def
                        { setCookieName = "flavor"
                        , setCookieValue = "chocolate-chip" }
                    default_time = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setCookieJar =<< receiveSetCookie setCookie req default_time True <$> getCookieJar
                        responseBody <$> makeRequestLbs req
                killThread tid
                elbs @?= "nom-nom-nom"
{- http-conduit-1.9
            it "user-defined cookies in req survive redirects" $ do
                tid <- forkIO $ run 3019 app
                req <- parseUrl "http://127.0.0.1:3019/cookie_redir2"
                let setCookie = def
                        { setCookieName = "flavor"
                        , setCookieValue = "chocolate-chip" }
                    default_time = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        cjar <- receiveSetCookie setCookie req default_time True <$> getCookieJar
                        let request = fst $ insertCookiesIntoRequest req cjar default_time
                        responseBody <$> makeRequestLbs request
                killThread tid
                liftIO $ elbs @?= "nom-nom-nom"
-}
            it "can save and load cookie jar" $ do
                tid <- forkIO $ run 3011 app
                request1 <- parseUrl "http://127.0.0.1:3011/cookies"
                request2 <- parseUrl "http://127.0.0.1:3011/print-cookies"
                (lbs1, lbs2) <- withManager $ \manager -> do
                    browse manager $ do
                        _ <- makeRequestLbs request1
                        cookie_jar <- getCookieJar
                        setCookieJar def
                        lbs1 <- responseBody <$> makeRequestLbs request2
                        setCookieJar cookie_jar
                        lbs2 <- responseBody <$> makeRequestLbs request2
                        return (lbs1, lbs2)
                killThread tid
                when (lbs1 /= "" || lbs2 /= "flavor=chocolate-chip") $
    				error "Cookie jar got garbled up!"
            it "user agent sets correctly" $ do
                tid <- forkIO $ run 3012 app
                request <- parseUrl "http://127.0.0.1:3012/useragent"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setDefaultHeader "User-Agent" $ Just "abcd"
                        makeRequestLbs request
                killThread tid
                when (responseBody elbs /= "abcd") $
					error "Should have gotten the user agent back!"
            it "default headers propagate" $ do
                tid <- forkIO $ run 3012 app
                request <- parseUrl "http://127.0.0.1:3012/useragent"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setDefaultHeader "User-Agent" $ Just "abcd"
                        makeRequestLbs request
                killThread tid
                responseBody elbs @?= "abcd"
            it "default headers get overriden" $ do
                tid <- forkIO $ run 3012 app
                request <- parseUrl "http://127.0.0.1:3012/useragent"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setDefaultHeader "User-Agent" $ Just "bwahaha"
                        makeRequestLbs request{requestHeaders = [(hUserAgent, "abcd")]
                killThread tid
                responseBody elbs @?= "abcd"
            it "user agent overrides" $ do
                tid <- forkIO $ run 3012 app
                request <- parseUrl "http://127.0.0.1:3012/useragent"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setOverrideHeader hUserAgent $ Just $ utf8String "abcd"
                        makeRequestLbs request{requestHeaders = [(hUserAgent, "bwahaha")]}
                killThread tid
                responseBody elbs @?= "abcd"
            it "doesn't override additional headers" $ do
                tid <- forkIO $ run 3012 app
                request <- parseUrl "http://127.0.0.1:3012/accept"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        insertOverrideHeader ("User-Agent", "http-conduit")
                        insertOverrideHeader ("Connection", "keep-alive")
                        makeRequestLbs request{requestHeaders = [("User-Agent", "another agent"), ("Accept", "everything/digestible")]}
                killThread tid
                when (lazyToStrict (responseBody elbs) /= "everything/digestible") $
					error "Shouldn't have deleted Accept header!"
            it "withOverrideHeader: doesn't override additional headers" $ do
                tid <- forkIO $ run 3012 app
                request1 <- parseUrl "http://127.0.0.1:3012/accept"
                request2 <- parseUrl "http://127.0.0.1:3012/useragent"
                (lbs1, lbs2) <- withManager $ flip browse $ do
                    insertOverrideHeader ("User-Agent", "another agent")
                    withOverrideHeader ("User-Agent", "http-conduit") $
                        insertOverrideHeader ("Accept", "everything/digestible")
                    r1 <- responseBody <$> makeRequestLbs request1
                    r2 <- responseBody <$> makeRequestLbs request2
                    return (r1,r2)
                killThread tid
                when (lbs1 /= "everything/digestible") $
                    error "Shouldn't have deleted Accept header!"
                when (lbs2 /= "another agent") $
                    error "Shouldn't have overriden user agent!"
            it "authorities get set correctly" $ do
                tid <- forkIO $ run 3013 app
                request <- parseUrl "http://127.0.0.1:3013/authorities"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setAuthorities $ const $ Just (user, pass)
                        makeRequestLbs request
                killThread tid
                if (lazyToStrict $ responseBody elbs) /= (utf8String "Basic " `S.append` (encode $ user `S.append` ":" `S.append` pass))
                     then error "Authorities didn't get set correctly!"
                     else return ()
            it "can follow redirects" $ do
                tid <- forkIO $ run 3014 app
                request <- parseUrl "http://127.0.0.1:3014/redir1"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setMaxRedirects $ Just 2
                        makeRequestLbs request
                killThread tid
                if (lazyToStrict $ responseBody elbs) /= dummy
                     then error "Should be able to follow 2 redirects"
                     else return ()
            it "max redirects fails correctly" $ do
                tid <- forkIO $ run 3015 app
                request <- parseUrl "http://127.0.0.1:3015/redir1"
                elbs <- try $ withManager $ \manager -> do
                    browse manager $ do
                        setMaxRedirects $ Just 1
                        makeRequestLbs request
                killThread tid
                case elbs of
                     Left (TooManyRedirects _) -> return ()
                     _ -> error "Shouldn't have followed all those redirects!"
            it "Retry fails correctly when it is too low" $ do
                writeIORef ref True
                tid <- forkIO $ run 3016 $ appWithSideEffect ref
                request <- parseUrl "http://127.0.0.1:3016/"
                elbs <- try $ withManager $ \manager -> do
                    browse manager $ do
                        setMaxRetryCount 0
                        makeRequestLbs request
                killThread tid
                case elbs of
                     Left (StatusCodeException _ _) -> return ()
                     _ -> error "1 redirect shouldn't be enough!"
            it "Makes multiple retries" $ do
                writeIORef ref True
                tid <- forkIO $ run 3017 $ appWithSideEffect ref
                request <- parseUrl "http://127.0.0.1:3017/"
                elbs <- withManager $ \manager -> do
                    browse manager $ do
                        setMaxRetryCount 1
                        makeRequestLbs request
                killThread tid
                if responseBody elbs /= success
                     then error "Didn't retry failed request"
                     else return ()
            it "throws statusCodeException, when maxRedirects=0" $ do
                tid <- forkIO $ run 3015 app
                request <- parseUrl "http://127.0.0.1:3015/redir1"
                elbs <- try $ withManager $ \manager -> do
                    browse manager $ do
                        setMaxRedirects $ Just 0
                        makeRequestLbs request
                killThread tid
                case elbs of
                     Left StatusCodeException{} -> return ()
                     _ -> error "Should've thrown StatusCodeException!"
            it "doesn't override redirectCount when maxRedirects=Nothing" $ do
                tid <- forkIO $ run 3015 app
                request <- parseUrl "http://127.0.0.1:3015/redir1"
                elbs <- try $ withManager $ \manager -> do
                    browse manager $ do
                        setMaxRedirects Nothing
                        makeRequestLbs request{redirectCount = 0}
                killThread tid
                case elbs of
                     Left StatusCodeException{} -> return ()
                     _ -> error "redirectCount /= 0!"
            it "overrides redirectCount when maxRedirects/=Nothing" $ do
                tid <- forkIO $ run 3015 app
                request <- parseUrl "http://127.0.0.1:3015/redir1"
                elbs <- try $ withManager $ \manager -> do
                    browse manager $ do
                        setMaxRedirects $ Just 0
                        makeRequestLbs request{redirectCount = 10}
                killThread tid
                case elbs of
                     Left StatusCodeException{} -> return ()
                     _ -> error "redirectCount should be 0!"
            it "uses checkStatus correctly" $ do
                tid <- forkIO $ run 3012 app
                request <- parseUrl "http://127.0.0.1:3012/useragent"
                elbs <- try $ withManager $ \manager -> do
                    browse manager $ do
                        setCheckStatus $ Just $  \ _ _ -> Just $ toException TestException
                        makeRequestLbs request
                killThread tid
                case elbs of
                    Left TestException -> return ()
                    _ -> error "Should have thrown an exception!"
            it "updates location" $ do
                tid <- forkIO $ run 3014 app
                request <- parseUrl "http://127.0.0.1:3014/"
                loc <- withManager $ \manager -> do
                    browse manager $ do
                        _ <- makeRequestLbs request
                        getLocation
                killThread tid
                if (maybe "" show loc) /= "http://127.0.0.1:3014/"
                     then error "Should have set the location"
                     else return ()
            it "updates location while following redirections" $ do
                tid <- forkIO $ run 3014 app
                request <- parseUrl "http://127.0.0.1:3014/redir1"
                loc <- withManager $ \manager -> do
                    browse manager $ do
                        setMaxRedirects $ Just 2
                        _ <- makeRequestLbs request
                        getLocation
                killThread tid
                if (maybe "" show loc) /= "http://127.0.0.1:3014/redir3"
                     then error "Should have updated the location when following 2 redirects"
                     else return ()
            it "follows relative references" $ do
                tid <- forkIO $ run 3014 app
                request1 <- parseUrl "http://127.0.0.1:3014/"
                (elbs1, elbs2) <- withManager $ \manager -> do
                    browse manager $ do
                        lbs1 <- makeRequestLbs request1
                        request2 <- parseRelativeUrl "redir3"
                        lbs2 <- makeRequestLbs request2
                        return (lbs1, lbs2)
                killThread tid
                if (lazyToStrict $ responseBody elbs1) /= utf8String "homepage" || (lazyToStrict $ responseBody elbs2) /= dummy
                     then error "Should have followed the relative reference"
                     else return ()
