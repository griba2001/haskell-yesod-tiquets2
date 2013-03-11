{-# LANGUAGE ConstraintKinds #-}
module Handler.UserMsg where

import Import as I

import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime) -- , UTCTime
import Data.Time.Calendar (Day(..), addDays) -- , diffDays
import Data.Maybe
import qualified Util.UtilForms as UF
import qualified Handler.UtilHandlers as UH
-- import Util.UtilQQHereDoc
-- import Database.Persist.GenericSql(rawSql)
import Database.Persist.Query.GenericSql (SqlPersist)
import qualified Data.Map as M
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Database.Esqueleto as Esql
import Control.Monad (when)
import qualified Util.UtilPaisos as UP


getMailR :: Int -> Handler RepHtml
getMailR pag = do
    muser <- maybeAuth
    let Entity userId user = fromJust muser  -- isAuthorized
        navPrimer = MailR 0
        navSegüent = MailR (pag +1)
        mbNavAnterior = if pag > 0
                         then Just $ MailR (pag -1)
                         else Nothing
        myOffset = pag * UF.correuEntradesPerPàgina
        
    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt
                                 
    mbLang <- UF.obtenirIdioma
    
    entMsgs <- runDB $ selectList [UserMsgIdUsuari I.==. userId] [Desc UserMsgPosted, OffsetBy myOffset, LimitTo UF.correuEntradesPerPàgina]
    let msgs = map entToPair entMsgs
    
    mydefaultLayout emptyWidget $ do
        setTitleI MsgUserListMsgTitle
        $(widgetFile "usermsg-llistar")

  where
    entToPair (Entity msgId val) = (msgId, val)

getMailViewR :: UserMsgId -> Handler RepHtml
getMailViewR userMsgId = do
    muser <- maybeAuth
    let Entity userId user = fromJust muser  -- isAuthorized
    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt

    mbLang <- UF.obtenirIdioma
    
    mbMsg <- runDB $ get userMsgId
    case mbMsg of
         Nothing -> notFound
         Just msg
                 | userMsgIdUsuari msg /= userId -> notFound
                 | otherwise -> do
                        runDB $ I.update userMsgId [UserMsgLlegit I.=. True]
                        mydefaultLayout emptyWidget $ do
                          setTitleI MsgUserVisitFlightTitle
                          $(widgetFile "usermsg-visitar")

