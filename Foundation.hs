{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes, StandaloneDeriving, DeriveDataTypeable #-}
module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , getExtra
    , idiomesSuportats
    , mydefaultLayout
    , emptyWidget
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
-- import Yesod.Auth.BrowserId
-- import Yesod.Auth.GoogleEmail
import Yesod.Auth.Email
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
-- import Yesod.Dispatch (PathPiece)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Data.Maybe 
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Yesod.Auth.Message as AM
import qualified Yesod.Auth (AuthPlugin(..), AuthRoute)
import qualified Data.List as L
import Util.MissatgesAuth (catalanAuthMessage)
import Util.MissatgesBreadCrumbs
import Data.Map (Map)
-- import Model.UtilAdmins

-- import Control.Monad.STM (STM, atomically)
-- import Control.Concurrent.STM.TVar (TVar, newTVar)

idiomesSuportats :: [Text]
idiomesSuportats = ["ca","en"]

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , appDestsRevMap :: Map Text Text  -- gabriel riba i faura
    -- , dadesGlobals :: TVar DadesGlobals
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")


type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
            mydefaultLayout emptyWidget widget
{-            
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuth
        -- req <- getRequest
        ésAdmin <- runDB $ userÉsAdmin muser
        mbCurrentRoute <- getCurrentRoute
        routeToMaster <- getRouteToMaster
        alreadyExpired
        setHeader "Cache-Control" "private, no-cache, no-store"

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_estil_meu_css
            -- addScript $ StaticR js_myscript_js
            -- loginHtml <- (apLogin authEmail) AuthR
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")
        -}
        
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

            
    isAuthorized UserInfoR _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized

    isAuthorized UserAddFlightR _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized
            
    isAuthorized (UserVisitFlightR _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized

    isAuthorized (UserVisitOwnFlightR _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized
            
    isAuthorized (ElsMeusVolsR _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized

    isAuthorized (UserSendInterestR _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized
            
    isAuthorized (MailR _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized
            
    isAuthorized (MailViewR _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized

    isAuthorized CercarUsrsR _ = do
        mauth <- maybeAuth
        ésAdmin <- runDB $ userÉsAdmin mauth
        if ésAdmin
            then return Authorized
            else return AuthenticationRequired
            
    isAuthorized (CercarUsrsResultR _ _ _ _ _ _) _ = do
        mauth <- maybeAuth
        ésAdmin <- runDB $ userÉsAdmin mauth
        if ésAdmin
            then return Authorized
            else return AuthenticationRequired

    isAuthorized (UserInfoAdminVisitarR _ ) _ = do
        mauth <- maybeAuth
        ésAdmin <- runDB $ userÉsAdmin mauth
        if ésAdmin
            then return Authorized
            else return AuthenticationRequired

    isAuthorized (UserInfoAdminLlistarArticlesR _ _ ) _ = do
        mauth <- maybeAuth
        ésAdmin <- runDB $ userÉsAdmin mauth
        if ésAdmin
            then return Authorized
            else return AuthenticationRequired
            
    isAuthorized _ _ = return Authorized
            
-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    onLogin = do
            
            muser <- maybeAuth
            case muser of
                 Nothing -> return ()
                 Just (Entity userId _) -> do
                         mbUserInfo <- runDB $ getBy (UniqueUserInfoUserId userId)
                         case mbUserInfo of
                              Nothing -> redirect UserInfoR
                              Just (Entity _ UserInfo {..}) -> do
                                      setLanguage userInfoIdioma
                                      master <- getYesod
                                      setMessage $ toHtml $ renderMessage master [userInfoIdioma] (MsgWelcome userInfoNom)
            -- 

    {-
    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing
                -}

    -- Need to find the UserId for the given email address.
    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False False
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user
                
    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail {apLogin = authEmailLogin}]  -- GRF menys authBrowserId, authGoogleEmail

    authHttpManager = httpManager

    renderAuthMessage _ langs = case (filter (`elem` idiomesSuportats) . map (TS.take 2)) langs of
                                     "ca":_ -> catalanAuthMessage
                                     _ -> AM.defaultMessage


-- authEmailLogin :: forall s. (AuthRoute -> Route m) -> GWidget s m ()
authEmailLogin = \tm ->
        [whamlet|
$newline never
<form method="post" action="@{tm loginR}">
    <table>
        <tr>
            <th>_{AM.Email}
            <td>
                <input type="email" name="email">
        <tr>
            <th>_{AM.Password}
            <td>
                <input type="password" name="password">
        <tr>
            <td colspan="2">
                <input type="submit" value=_{AM.LoginViaEmail}>
                <a href="@{tm registerR}"> _{MsgFnd_IDontHaveAnAccount}
|]

instance YesodAuthEmail App where
    type AuthEmailId App = UserId
    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False False

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _u -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                }
    getEmail = runDB . fmap (fmap userEmail) . get    
    
-- instance PathPiece (AuthEmailId App) 


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

instance YesodBreadcrumbs App where
        
    breadcrumb HomeR = return $ (bc_HomeR, Nothing)
    breadcrumb UserInfoR = return $ (bc_UserInfoR, Just HomeR)
    
    breadcrumb UserAddFlightR = return (bc_UserAddFlightR, Just HomeR)
    breadcrumb (UserEditFlightR _) = return (bc_UserEditFlightR, Just HomeR)
    breadcrumb (UserVisitFlightR _) = return (bc_UserVisitFlightR, Just HomeR)
    breadcrumb (UserVisitOwnFlightR _) = return (bc_UserVisitOwnFlightR, Just HomeR)
    breadcrumb (UserSendInterestR _) = return (bc_UserSendInterestR, Just HomeR)
    
    breadcrumb (MailR _) = return (bc_MailR, Just HomeR)
    breadcrumb (MailViewR _) = return (bc_MailViewR, Just $ MailR 0)

    breadcrumb (ElsMeusVolsR _) = return (bc_ElsMeusVolsR, Just HomeR)
    breadcrumb (TotsElsVolsR _) = return (bc_TotsElsVolsR, Just HomeR)
    breadcrumb (CercarVolsR) = return (bc_CercarVolsR, Just HomeR)
    breadcrumb (CercarVolsResultR _ _ _ _) = return (bc_CercarVolsResultR, Just CercarVolsR)
    
    breadcrumb CercarUsrsR = return (bc_CercarUsrsR, Just HomeR)
    breadcrumb (CercarUsrsResultR _ _ _ _ _ _) = return (bc_CercarUsrsResultR, Just CercarUsrsR)
    
    breadcrumb (UserInfoAdminVisitarR _) = return (bc_UserInfoAdminVisitarR, Just CercarUsrsR)
    breadcrumb (UserInfoAdminLlistarArticlesR _ _) = return (bc_UserInfoAdminLlistarArticlesR, Just $ MailR 0)
    breadcrumb (AdminVisitFlightR _) = return (bc_AdminVisitFlightR, Just CercarUsrsR)
    
    breadcrumb GlobalsR = return (bc_GlobalsR, Just HomeR)
    breadcrumb CreditsR = return (bc_CreditsR, Just HomeR)
    
    breadcrumb _ = return ("", Nothing)

mydefaultLayout :: GWidget sub App () -> GWidget sub App () -> GHandler sub App RepHtml
mydefaultLayout widgetLeft widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuth
        -- req <- getRequest
        ésAdmin <- runDB $ userÉsAdmin muser
        mbCurrentRoute <- getCurrentRoute
        routeToMaster <- getRouteToMaster
        alreadyExpired
        setHeader "Cache-Control" "private, no-cache, no-store"

        (_,bcAncestres) <- breadcrumbs
        let bcWidget = $(widgetFile "breadcrumbs")

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_estil_meu_css
            -- addScript $ StaticR js_myscript_js
            -- loginHtml <- (apLogin authEmail) AuthR
            $(widgetFile "mydefault-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")
    