{-# LANGUAGE ConstraintKinds #-}
module Handler.UserVolList where

import Import as I

import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime) -- , UTCTime
import Data.Time.Calendar (Day(..), addDays) -- , diffDays
import Data.Maybe as MB
import qualified Util.UtilForms as UF
-- import Util.UtilQQHereDoc
-- import Database.Persist.GenericSql(rawSql)
import Database.Persist.Query.GenericSql (SqlPersist)
import qualified Data.Map as M
import Data.Map (Map)
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


                  
getElsMeusVolsR :: Int -> Handler RepHtml
getElsMeusVolsR pag = do
    req <- getRequest
    master <- getYesod
    muser <- maybeAuth
    let Entity userId user = fromJust muser  -- isAuthorized

    let navPrimer = ElsMeusVolsR 0
        navSegüent = ElsMeusVolsR (pag +1)
        mbNavAnterior = if pag > 0
                         then Just $ ElsMeusVolsR (pag -1)
                         else Nothing
        myOffset = pag * UF.volsEntradesPerPàgina
    
    result <- runDB $ select $ from $ \(ua `InnerJoin` av) -> do
                on ((ua ^. UserArtIdArtVol) Esql.==. just (av ^. ArtVolId))
                where_ (ua ^. UserArtIdUsuari Esql.==. val userId)
                -- where_ (ua ^. UserArtEstat Esql.==. val EstArt_Exposat)
                orderBy [desc (ua ^. UserArtPosted)]
                offset $ fromIntegral myOffset
                limit $ fromIntegral UF.volsEntradesPerPàgina
                return (ua, av)
                
    let vols = map userVolFromUserArtEnts result
    let destMap = appDestsRevMap master
    
                
    -- vols <- runDB $ selectList [UserVolIdUsuari ==. userId] [Desc UserVolPosted, OffsetBy offset, LimitTo UF.volsEntradesPerPàgina]

    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt

    if MB.isJust muser && MB.isNothing mbUserInfo
         then do
                setMessageI MsgErrRequeixPerfilUsuari
                redirect UserInfoR
         else do
                mbLang <- UF.obtenirIdioma
                mbCurrentRoute <- getCurrentRoute
                routeToMaster <- getRouteToMaster
                {-
                case mbCurrentRoute of
                     Nothing -> return ()
                     Just cr -> setSession "CurrRoute" $ serialitza $
                     -}

                let ésModificable = True
                mydefaultLayout emptyWidget $ do
                        setTitleI MsgUserListMyFlightsTitle
                        $(widgetFile "uservol-llistar-meus")
                        
getTotsElsVolsR :: Int -> Handler RepHtml
getTotsElsVolsR pag = do
    -- req <- getRequest
    -- master <- getYesod
  if pag < 0
    then notFound
    else do
      muser <- maybeAuth
      diaDAvui <- liftIO $ UF.today ()
      let navPrimer = TotsElsVolsR 0
          navSegüent = TotsElsVolsR (pag +1)
          mbNavAnterior = if pag > 0
                         then Just $ TotsElsVolsR (pag -1)
                         else Nothing
          myOffset = pag * UF.volsEntradesPerPàgina
    
      {-
      let avuiFa6mesos = addDays (-180) diaDAvui
          utctAvuiFa6mesos = UTCTime avuiFa6mesos $ secondsToDiffTime 0
          filtresDeSelect = [UserVolPosted >=. utctAvuiFa6mesos] ++
                    case muser of
                       Nothing -> []
                       Just (Entity userId _) -> [UserVolIdUsuari !=. userId]
                       -}
      let títol = case muser of
                       Nothing -> MsgUserListAllFlightsTitle
                       Just _ -> MsgUserListOthersFlightsTitle
                       
      mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt

      if MB.isJust muser && MB.isNothing mbUserInfo
         then do
                 setMessageI MsgErrRequeixPerfilUsuari
                 redirect UserInfoR
         else do
                       
                mbLang <- UF.obtenirIdioma
                
                -- vols <- runDB $ selectList filtresDeSelect [Desc UserVolPosted, OffsetBy offset, LimitTo UF.volsEntradesPerPàgina]
                result <- runDB $ select $ from $ \(av `InnerJoin` ua `InnerJoin` usr) -> do
                                on ((ua ^. UserArtIdUsuari) Esql.==. (usr ^. UserId))
                                on ((ua ^. UserArtIdArtVol) Esql.==. just (av ^. ArtVolId))
                                where_ ((usr ^. UserVeto) Esql.==. val False)
                                where_ ((ua ^. UserArtEstat) Esql.==. val EstArt_Exposat)
                                where_ (ua ^. UserArtData Esql.>=. val diaDAvui)
                                when (isJust muser) $ where_ (ua ^. UserArtIdUsuari Esql.!=. val (entityKey $ fromJust muser))
                                orderBy [desc (ua ^. UserArtPosted)]
                                offset $ fromIntegral myOffset
                                limit $ fromIntegral UF.volsEntradesPerPàgina
                                return (usr, ua, av)

                let result2 = map (\(_,a,b) -> (a,b)) (result::[(Entity User, Entity UserArt, Entity ArtVol)])
                    
                let vols = map userVolFromUserArtEnts result2
                master <- getYesod
                let destMap = appDestsRevMap master
                    
                mbCurrentRoute <- getCurrentRoute
                routeToMaster <- getRouteToMaster

                let ésModificable = False
                volsRecents <- obtenirVolsRecents
                
                mydefaultLayout (widgetVolsRecents destMap muser mbLang volsRecents)
                                   (widgetTotsElsVols destMap muser títol mbLang ésModificable navPrimer navSegüent mbNavAnterior vols)
                                
                {-    
                defaultLayout $ do
                        setTitleI títol
                        $(widgetFile "uservol-llistar")
                        -}



widgetTotsElsVols :: Map Text Text -> Maybe (Entity User) -> AppMessage -> Maybe Text -> Bool ->
                     Route App -> Route App -> Maybe (Route App) -> [(UserArtId, UserVol)] -> GWidget sub App ()
widgetTotsElsVols destMap muser títol mbLang ésModificable navPrimer navSegüent mbNavAnterior vols = do
                        setTitleI títol
                        $(widgetFile "uservol-llistar")

obtenirVolsRecents :: Handler [(UserArtId, UserVol)]
obtenirVolsRecents = do
        muser <- maybeAuth
        mbLang <- UF.obtenirIdioma
        diaDAvui <- liftIO $ UF.today ()

        -- vols <- runDB $ selectList filtresDeSelect [Desc UserVolPosted, OffsetBy offset, LimitTo UF.volsEntradesPerPàgina]
        result <- runDB $ select $ from $ \(av `InnerJoin` ua `InnerJoin` usr) -> do
                on ((ua ^. UserArtIdUsuari) Esql.==. (usr ^. UserId))
                on ((ua ^. UserArtIdArtVol) Esql.==. just (av ^. ArtVolId))
                where_ ((usr ^. UserVeto) Esql.==. val False)
                where_ ((ua ^. UserArtEstat) Esql.==. val EstArt_Exposat)
                where_ (ua ^. UserArtData Esql.>=. val diaDAvui)
                when (isJust muser) $ where_ (ua ^. UserArtIdUsuari Esql.!=. val (entityKey $ fromJust muser))
                orderBy [desc (ua ^. UserArtPosted)]
                limit 3
                return (usr, ua, av)

        let result2 = map (\(_,a,b) -> (a,b)) (result::[(Entity User, Entity UserArt, Entity ArtVol)])

        let volsRecents = map userVolFromUserArtEnts result2
        return volsRecents


widgetVolsRecents :: Map Text Text -> Maybe (Entity User) -> Maybe Text -> [(UserArtId, UserVol)] -> GWidget sub App ()
widgetVolsRecents destMap muser mbLang volsRecents = do
        $(widgetFile "uservol-llistar-recents")
                        