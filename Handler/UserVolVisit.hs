{-# LANGUAGE ConstraintKinds #-}
module Handler.UserVolVisit where

import Import as I

import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime) -- , UTCTime
import Data.Time.Calendar (Day(..), addDays) -- , diffDays
import Data.Maybe as MB
import qualified Util.UtilForms as UF
-- import Util.UtilQQHereDoc
-- import Database.Persist.GenericSql(rawSql)
import Database.Persist.Query.GenericSql (SqlPersist)
import qualified Data.Map as M
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Database.Esqueleto as Esql
import Control.Monad (when)
import Text.Julius (juliusFile)
import Text.Hamlet (hamletFile)
import qualified Handler.UtilHandlers as UH
import Yesod.Form.Types (FormMessage(..))
import Handler.UserVol
import qualified Util.UtilPaisos as UP
{-
import "hsp" HSP.XML.PCDATA (escape)
import Data.Text.Lazy.Builder (toLazyText)
-}


                  

getUserVisitFlightR :: UserArtId -> Handler RepHtml
getUserVisitFlightR artId = do
        
    muser <- maybeAuth
    mbDG <- runDB llegirGlobals
    let dadesGlobals = fromJust mbDG
    
    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt
                                 
    mbLang <- UF.obtenirIdioma
                         
    mbUserArt <- runDB $ get artId
    case mbUserArt of
       Nothing -> notFound
       Just art
                | userArtTipusArt art /= TipArtVol -> notFound
                | not $ isJust $ userArtIdArtVol art -> notFound
                | otherwise -> do
                       let idArtVol = fromJust $ userArtIdArtVol art
                       mbArtVol <- runDB $ get idArtVol
                       case mbArtVol of
                         Nothing -> notFound
                         Just artVol -> do
                           let vol = userVolFromUserArt art artVol
                           master <- getYesod
                           let destMap = appDestsRevMap master
                           mydefaultLayout emptyWidget $ do
                               setTitleI MsgUserVisitFlightTitle
                               $(widgetFile "uservol-visitar-titol")
                               $(widgetFile "uservol-visitar-detalls-venda")
                               $(widgetFile "boto-enviar-msg-interes")

getUserVisitOwnFlightR :: UserArtId -> Handler RepHtml
getUserVisitOwnFlightR artId = do

    muser <- maybeAuth
    let Entity userId user = fromJust muser  -- isAuthorized
    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt
                                 
    mbLang <- UF.obtenirIdioma

    mbUserArt <- runDB $ get artId
    case mbUserArt of
       Nothing -> notFound
       Just art
                | userArtTipusArt art /= TipArtVol -> notFound
                | not $ isJust $ userArtIdArtVol art -> notFound
                | userArtIdUsuari art /= userId -> notFound
                | otherwise -> do
                       let idArtVol = fromJust $ userArtIdArtVol art
                       mbArtVol <- runDB $ get idArtVol
                       case mbArtVol of
                         Nothing -> notFound
                         Just artVol -> do
                           let vol = userVolFromUserArt art artVol
                           master <- getYesod
                           let destMap = appDestsRevMap master
                           mydefaultLayout emptyWidget $ do
                               setTitleI MsgUserVisitFlightTitle
                               $(widgetFile "uservol-visitar-titol")
                               $(widgetFile "uservol-visitar-detalls-compra")
                               $(widgetFile "boto-back")

getAdminVisitFlightR :: UserArtId -> Handler RepHtml
getAdminVisitFlightR artId = do

    muser <- maybeAuth
    mbDG <- runDB llegirGlobals
    let dadesGlobals = fromJust mbDG
    
    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt

    mbLang <- UF.obtenirIdioma

    mbUserArt <- runDB $ get artId
    case mbUserArt of
       Nothing -> notFound
       Just art
                | userArtTipusArt art /= TipArtVol -> notFound
                | not $ isJust $ userArtIdArtVol art -> notFound
                | otherwise -> do
                       let idArtVol = fromJust $ userArtIdArtVol art
                       mbArtVol <- runDB $ get idArtVol
                       case mbArtVol of
                         Nothing -> notFound
                         Just artVol -> do
                           let vol = userVolFromUserArt art artVol
                           master <- getYesod
                           let destMap = appDestsRevMap master
                           mydefaultLayout emptyWidget $ do
                               setTitleI MsgUserVisitFlightTitle
                               $(widgetFile "uservol-visitar-titol")
                               $(widgetFile "uservol-visitar-detalls-compra")

        