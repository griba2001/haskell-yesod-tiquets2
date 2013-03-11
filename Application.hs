{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.UserInfo
-- import Handler.UserVol  -- no conté entrades
import Handler.SetLang
import Handler.CercarVols
import Handler.UserMsg
import Handler.UserVolSendInterest
import Handler.CercarUsuaris
import Handler.UtilHandlers
import Handler.Globals
import Handler.HXR_Aeroports
import Handler.UserVolAddEdit
import Handler.UserVolList
import Handler.UserVolVisit
import Handler.HPaisos
import qualified Handler.UtilHandlers as UH
import qualified Util.UtilForms as UF

-- import Control.Monad.STM (STM, atomically)
-- import Control.Concurrent.STM.TVar (TVar, newTVar)
-- import Model.UtilAdmins

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    destinacions <- flip (Database.Persist.Store.runPool dbconf) p $ UH.obtenirDestinacions Nothing
    let destMap = UF.destinacionsRevMap destinacions
    return $ App conf s p manager dbconf destMap

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
{-
obtenirDadesGlobals :: IO DadesGlobals
obtenirDadesGlobals = return $ DadesGlobals {dgComissió=0.20, dgAdmins=["griba2001@gmail.com", "gabi64@zotac-ion"]}
-}