{-# LANGUAGE ConstraintKinds, RecordWildCards #-}
module Handler.CercarUsuaris where

import Import as I

import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime) -- , UTCTime
import Data.Time.Calendar (Day(..), addDays, diffDays)
import Data.Maybe as MB
import qualified Util.UtilForms as UF
import qualified Handler.UtilHandlers as UH
-- import Util.UtilQQHereDoc
-- import Database.Persist.GenericSql(rawSql)
import qualified Data.Map as M
import Control.Monad (when)
import Database.Esqueleto as Esql
import Handler.UserVol (UserVol(..), userVolFromUserArtEnts)
import qualified Util.UtilPaisos as UP

cercarUsrsForm :: Form (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
cercarUsrsForm = renderTable $ (,,,,)
        <$> aopt textField (fieldSettingsLabel MsgUserInfoNom) Nothing
        <*> aopt textField (fieldSettingsLabel MsgUserInfoCognoms) Nothing
        <*> aopt textField (fieldSettingsLabel MsgUserInfoCodiPostal) Nothing
        <*> aopt textField (fieldSettingsLabel MsgUserInfoPoblació) Nothing
        <*> aopt (selectFieldList UP.països) (fieldSettingsLabel MsgUserInfoPaís) Nothing

getCercarUsrsR :: Handler RepHtml
getCercarUsrsR = do
    muser <- maybeAuth
    (tabulatedFormWidget, enctype) <- generateFormPost cercarUsrsForm
    mydefaultLayout emptyWidget $ do
        setTitleI MsgFlightSearchTitle
        $(widgetFile "tabulated-form")

postCercarUsrsR :: Handler RepHtml
postCercarUsrsR = do
    muser <- maybeAuth
    ((res, tabulatedFormWidget), enctype) <- runFormPost cercarUsrsForm
    case res of
        FormSuccess (mbNom, mbCognoms, mbCodiPostal, mbPoblació, mbPaís) -> do

                redirect $ CercarUsrsResultR (UF.mbTextToText mbNom) (UF.mbTextToText mbCognoms)
                                             (UF.mbTextToText mbCodiPostal) (UF.mbTextToText mbPoblació)
                                             (UF.mbTextToText mbPaís) 0
        _ -> do
                setMessageI MsgPleaseCorrect    
                mydefaultLayout emptyWidget $ do
                    $(widgetFile "tabulated-form")

diesCercaDates = 15 :: Integer

getCercarUsrsResultR :: Text -> Text -> Text -> Text -> Text -> Int -> Handler RepHtml
getCercarUsrsResultR txtNom txtCognoms txtCodiPostal txtPoblació txtPaís pag = do
    muser <- maybeAuth

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

                let     myOffset = pag * UF.usuarisEntradesPerPàgina

                        mbNom = UF.txtToMbText txtNom
                        mbCognoms = UF.txtToMbText txtCognoms
                        mbCodiPostal = UF.txtToMbText txtCodiPostal
                        mbPoblació = UF.txtToMbText txtPoblació
                        mbPaís = UF.txtToMbText txtPaís

                        filtreNom = case mbNom of
                                        Just txt -> [UserInfoNom `UH.icontains` txt]
                                        Nothing -> []

                        filtreCognoms = case mbCognoms of
                                        Just txt -> [UserInfoCognoms `UH.icontains` txt]
                                        Nothing -> []

                        filtreCodiPostal = case mbCodiPostal of
                                        Just txt -> [UserInfoCodiPostal `UH.icontains` txt]
                                        Nothing -> []

                        filtrePoblació = case mbPoblació of
                                        Just txt -> [UserInfoPoblació `UH.icontains` txt]
                                        Nothing -> []

                        filtrePaís = case mbPaís of
                                        Just txt -> [UserInfoPaís I.==. txt]
                                        Nothing -> []

                        filtres = filtreNom ++
                                filtreCognoms ++
                                filtreCodiPostal ++
                                filtrePoblació ++
                                filtrePaís

                result <- runDB $ selectList filtres [Asc UserInfoNom, Asc UserInfoCognoms, OffsetBy myOffset, LimitTo UF.volsEntradesPerPàgina]
                let usrs = map (\(Entity eid eval) -> (eid, eval)) (result :: [Entity UserInfo])

                let navPrimer = CercarUsrsResultR txtNom txtCognoms txtCodiPostal txtPoblació txtPaís 0
                    navSegüent = CercarUsrsResultR txtNom txtCognoms txtCodiPostal txtPoblació txtPaís (pag +1)
                    mbNavAnterior = if pag > 0
                                        then Just $ CercarUsrsResultR txtNom txtCognoms txtCodiPostal txtPoblació txtPaís (pag -1)
                                        else Nothing

                let ésModificable = False
                mydefaultLayout emptyWidget $ do
                        $(widgetFile "userinfo-llistar")
