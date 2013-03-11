{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, ConstraintKinds, ScopedTypeVariables #-}
module Handler.HPaisos where

import Import hiding (catch)
import Util.UtilPaisos as UP
import Util.UtilAeroports as UA
import Control.Monad
import Control.Exception (catch, SomeException)
import qualified Data.Text as T

import Database.Persist
import Database.Persist.GenericSql (SqlPersist, rawSql)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
import System.IO (readFile)

getPaisosR :: Handler RepHtml
getPaisosR = do
    result <- runDB carregaPaïsos
    defaultLayout $ do
            setTitle "Carrega països"
            (toWidget . toHtml) result

carregaPaïsos :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => SqlPersist m Text
carregaPaïsos = do
                  forM_ UP.països $ \(nom, codi) -> (insertUnique $ País codi nom) >> return ()
                  return "Correcte"


getCarregaAeroportsR :: Text -> Handler RepHtml
getCarregaAeroportsR term = do
        contingut <- liftIO $ readFile $ "docs/aeroports-"++ T.unpack term ++".csv"
        result <- runDB $ carregaAeroports contingut
        defaultLayout $ do
            setTitle "Carrega països"
            (toWidget . toHtml) result

carregaAeroports :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => String -> SqlPersist m Text
carregaAeroports contingut = do
        forM_ (UA.aeroports contingut) $ \(codi, població, provincia, aeroport, país) -> do
                let mbProvincia = if T.null provincia then Nothing else Just provincia
                let mbAeroport = if T.null aeroport then Nothing else Just aeroport
                insertUnique $ Aeroport codi població mbProvincia mbAeroport país
        return "Correcte"
        
                
