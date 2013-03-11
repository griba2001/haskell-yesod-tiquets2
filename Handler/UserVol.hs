{-# LANGUAGE ConstraintKinds #-}
module Handler.UserVol (
  UserVol(..),
  userArtFromUserVol, artVolFromUserVol,
  userVolFromUserArt, userVolFromUserArtEnts,
  userArtUpd, userArtUpdEstat, artVolUpd,
  widgetJSDatePick
) where

import Import as I

import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime) -- , UTCTime
import Data.Time.Calendar (Day(..), addDays) -- , diffDays
-- import Data.Maybe as MB
import qualified Util.UtilForms as UF
import Database.Persist.Query.GenericSql (SqlPersist)
-- import qualified Data.Map as M
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
-- import Database.Esqueleto as Esql
-- import Control.Monad (when)
import Text.Julius (juliusFile)
import Text.Hamlet (hamletFile)
-- import qualified Handler.UtilHandlers as UH
-- import Yesod.Form.Types (FormMessage(..))

data UserVol = UserVol {userVolIdUsuari::UserId, userVolPosted::UTCTime,
                        userVolOrigenPaís::Text,
                        -- userVolOrigenPoblació::Text,
                        userVolOrigenAeroport::Text,
                        userVolDestíPaís::Text,
                        -- userVolDestíPoblació::Text,
                        userVolDestíAeroport::Text,
                        userVolDataAnada::Day, userVolNumVolAnada::Text,
                        userVolDataTornada::Maybe Day, userVolNumVolTornada::Maybe Text,
                        userVolNombreDeSeients::Int,
                        userVolLíniaAèria::Text,
                        userVolNumReserva::Text,
                        userVolWebOnVaFerLaReserva::Text,
                        userVolPreu::Double, userVolMoneda::Moneda,
                        -- userVolÉsPreuNegociable::Bool,
                        userVolComentaris::Maybe Textarea,
                        userVolEstat::EstatArt}

userArtFromUserVol :: UserVol -> ArtVolId -> UserArt                        
userArtFromUserVol UserVol {..} idArtVol = UserArt { userArtIdUsuari = userVolIdUsuari,
                                      userArtPosted = userVolPosted,
                                      userArtData = userVolDataAnada,
                                      userArtPreu = userVolPreu,
                                      userArtMoneda = userVolMoneda,
                                      -- userArtÉsPreuNegociable = userVolÉsPreuNegociable,
                                      userArtComentaris = userVolComentaris,        
                                      userArtTipusArt = TipArtVol,
                                      userArtIdArtVol = Just idArtVol,
                                      userArtIdArtEnt = Nothing,
                                      userArtEstat = userVolEstat        }

artVolFromUserVol :: Map Text Text -> UserVol -> ArtVol
artVolFromUserVol destMap UserVol {..} = ArtVol { artVolOrigenPaís = userVolOrigenPaís,
                                    -- artVolOrigenPoblació = userVolOrigenPoblació,
                                    artVolOrigenPoblació = UF.nomDest destMap userVolOrigenAeroport,
                                    artVolOrigenAeroport = userVolOrigenAeroport,
                                    artVolDestíPaís = userVolDestíPaís,
                                    -- artVolDestíPoblació = userVolDestíPoblació,
                                    artVolDestíPoblació = UF.nomDest destMap userVolDestíAeroport,
                                    artVolDestíAeroport = userVolDestíAeroport,
                                    artVolNumVol = userVolNumVolAnada,
                                    artVolDataTornada = userVolDataTornada,
                                    artVolNumVolTornada = userVolNumVolTornada,
                                    artVolLíniaAèria = userVolLíniaAèria,
                                    artVolNumReserva = userVolNumReserva,
                                    artVolWebOnVaFerLaReserva = userVolWebOnVaFerLaReserva,
                                    artVolNombreDeSeients = userVolNombreDeSeients}

userVolFromUserArt :: UserArt -> ArtVol -> UserVol
userVolFromUserArt UserArt {..} ArtVol {..} = UserVol {userVolIdUsuari = userArtIdUsuari, userVolPosted = userArtPosted,
                                                       userVolOrigenPaís = artVolOrigenPaís,
                                                       -- userVolOrigenPoblació = artVolOrigenPoblació,
                                                       userVolOrigenAeroport = artVolOrigenAeroport,
                                                       userVolDestíPaís = artVolDestíPaís,
                                                       -- userVolDestíPoblació = artVolDestíPoblació,
                                                       userVolDestíAeroport = artVolDestíAeroport,
                                                       userVolDataAnada = userArtData, userVolNumVolAnada = artVolNumVol,
                                                       userVolDataTornada = artVolDataTornada, userVolNumVolTornada = artVolNumVolTornada,
                                                       userVolPreu = userArtPreu,
                                                       userVolMoneda = userArtMoneda,
                                                       -- userVolÉsPreuNegociable = userArtÉsPreuNegociable,
                                                       userVolComentaris = userArtComentaris,
                                                       userVolLíniaAèria = artVolLíniaAèria,
                                                       userVolNumReserva = artVolNumReserva,
                                                       userVolWebOnVaFerLaReserva = artVolWebOnVaFerLaReserva,        
                                                       userVolNombreDeSeients = artVolNombreDeSeients,
                                                       userVolEstat = userArtEstat}

userVolFromUserArtEnts :: (Entity UserArt, Entity ArtVol) -> (UserArtId, UserVol)
userVolFromUserArtEnts (Entity artId userArt, Entity _ artVol) = (artId, userVolFromUserArt userArt artVol)


            
artVolUpd :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => Map Text Text -> ArtVolId -> UserVol -> SqlPersist m ()
artVolUpd destMap artVolId UserVol {..} = I.update artVolId [ArtVolOrigenPaís I.=. userVolOrigenPaís,
                                         ArtVolOrigenPoblació I.=. UF.nomDest destMap userVolOrigenAeroport,
                                         ArtVolOrigenAeroport I.=. userVolOrigenAeroport,
                                         ArtVolDestíPaís I.=. userVolDestíPaís,
                                         ArtVolDestíPoblació I.=. UF.nomDest destMap userVolDestíAeroport,
                                         ArtVolDestíAeroport I.=. userVolDestíAeroport,
                                         ArtVolNumVol I.=. userVolNumVolAnada,
                                         ArtVolDataTornada I.=. userVolDataTornada,
                                         ArtVolNumVolTornada I.=. userVolNumVolTornada,
                                         ArtVolNombreDeSeients I.=. userVolNombreDeSeients,
                                         ArtVolLíniaAèria I.=. userVolLíniaAèria,
                                         ArtVolNumReserva I.=. userVolNumReserva,
                                         ArtVolWebOnVaFerLaReserva I.=. userVolWebOnVaFerLaReserva]

userArtUpd :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => UserArtId -> UserVol -> SqlPersist m ()
userArtUpd userArtId UserVol {..} = I.update userArtId [UserArtData I.=. userVolDataAnada,
                                                        UserArtPreu I.=. userVolPreu,
                                                        UserArtMoneda I.=. userVolMoneda,
                                                        -- UserArtÉsPreuNegociable I.=. userVolÉsPreuNegociable,
                                                        UserArtComentaris I.=. userVolComentaris]

userArtUpdEstat :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => UserArtId -> EstatArt -> SqlPersist m ()                                                        
userArtUpdEstat userArtId estatArt = I.update userArtId [UserArtEstat I.=. estatArt]

widgetJSDatePick :: Maybe Text -> GWidget sub App ()
widgetJSDatePick mbLang = do
        addStylesheetAttrs (StaticR jsdatepick_calendar_jsDatePick_ltr_min_css) [("media","all")]
        toWidgetHead [hamlet|
<script type="text/javascript" src="@{StaticR jsdatepick_calendar_jsDatePick_min_1_3_js}">
|]
        toWidgetHead $(juliusFile "templates/jsdatepick-idiomes.julius")
        toWidgetHead [hamlet|
<script type="text/javascript" src="@{StaticR js_jsdatepick_afegir_vol_js}">
|]
        return ()

