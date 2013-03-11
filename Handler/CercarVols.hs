{-# LANGUAGE ConstraintKinds, RecordWildCards #-}
module Handler.CercarVols where

import Import as I

import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime) -- , UTCTime
import Data.Time.Calendar (Day(..), addDays, diffDays)
import Data.Maybe
import qualified Util.UtilForms as UF
-- import Util.UtilQQHereDoc
-- import Database.Persist.GenericSql(rawSql)
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (when)
import Database.Esqueleto as Esql
import Handler.UserVol (UserVol(..), userVolFromUserArtEnts)
import Text.Julius (juliusFile)
import qualified Handler.UtilHandlers as UH
import qualified Util.UtilPaisos as UP

{-
        <$> (formToAForm $ mreq (selectFieldList UF.destinacions) (fieldSettingsLabel MsgLegendOrigen) Nothing >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> aopt (selectFieldList UF.destinacions) (fieldSettingsLabel MsgLegendDestí) Nothing
        <*> aopt dayField (fieldSettingsLabel MsgUserVolDataAnada) {fsAttrs = [("autocomplete","off")]} Nothing
-}

cercarVolsForm :: [(Text,Text)] -> [(Text,Text)] -> Form (Text, Maybe Text, Maybe Day)
cercarVolsForm destinacionsInicialsOrigen destinacionsInicialsDestí extra = do
        (userVolOrigenPaís_Res, userVolOrigenPaís_View) <- mreq (selectField $ UH.myTextOptionsPairs UP.països) ((fieldSettingsLabel MsgUserVolOrigenPaís) {fsAttrs = [("onchange","enCanviarPais(this.value,'h3', false)")]}) Nothing >>= (return . fmap UF.vistaAmbEstrella)
        (userVolOrigenAeroport_Res, userVolOrigenAeroport_View) <- mreq (selectField $ UH.myTextTextOptionsPairs $ destinacionsInicialsOrigen) {fieldParse = parseHelper comprovaDestOrigen} (fieldSettingsLabel MsgUserVolOrigenAeroport) Nothing >>= (return . fmap UF.vistaAmbEstrella)

        (userVolDestíPaís_Res, userVolDestíPaís_View) <- mopt (selectField $ UH.myTextOptionsPairs UP.països) ((fieldSettingsLabel MsgUserVolDestíPaís) {fsAttrs = [("onchange","enCanviarPais(this.value,'h5', true)")]}) Nothing
        (userVolDestíAeroport_Res, userVolDestíAeroport_View) <- mopt (selectField $ UH.myTextTextOptionsPairs $ destinacionsInicialsDestí) {fieldParse = UF.parseHelper comprovaDestDestí} (fieldSettingsLabel MsgUserVolDestíAeroport) Nothing 
        (userVolDataAnada_Res, userVolDataAnada_View) <- mopt dayField (fieldSettingsLabel MsgUserVolDataAnada) {fsAttrs = [("autocomplete","off")]} Nothing

        let form_Res = (,,) <$> userVolOrigenAeroport_Res <*> userVolDestíAeroport_Res <*> userVolDataAnada_Res
        let form_View = UH.massRows [(Just MsgLegendOrigen,[userVolOrigenPaís_View, userVolOrigenAeroport_View]),
                                     (Just MsgLegendDestí,[userVolDestíPaís_View, userVolDestíAeroport_View]),
                                     (Just MsgLegendAnada, [userVolDataAnada_View])
                                     ]
                
        let widget = [whamlet|
        #{extra}
        ^{form_View}
|]
        return (form_Res, widget)
  where
    comprovaDestOrigen "none" = Left MsgValueRequired
    comprovaDestOrigen txt = Right txt
         
    comprovaDestDestí txt = Right txt
        

widgetJSDatePick :: Maybe Text -> GWidget sub App ()
widgetJSDatePick mbLang = do
        addStylesheetAttrs (StaticR jsdatepick_calendar_jsDatePick_ltr_min_css) [("media","all")]
        toWidgetHead [hamlet|
<script type="text/javascript" src="@{StaticR jsdatepick_calendar_jsDatePick_min_1_3_js}">
|]
        toWidgetHead $(juliusFile "templates/jsdatepick-idiomes.julius")
        toWidgetHead [hamlet|
<script type="text/javascript" src="@{StaticR js_jsdatepick_cercar_vols_js}">
|]
        return ()
        
getCercarVolsR :: Handler RepHtml
getCercarVolsR = do
    mbLang <- UF.obtenirIdioma 
    let paísOrigen = (snd . L.head) UP.països
        paísDestí = (snd . L.head) UP.països

    destinacionsInicialsOrigen <- runDB $ UH.obtenirDestinacions $ Just paísOrigen
    destinacionsInicialsDestí <- runDB $ UH.obtenirDestinacions $ Just paísDestí
    
    (tabulatedFormWidget, enctype) <- generateFormPost $ cercarVolsForm destinacionsInicialsOrigen destinacionsInicialsDestí
    mydefaultLayout emptyWidget $ do
        setTitleI MsgFlightSearchTitle
        toWidgetHead $(juliusFile "templates/uservol-form.julius")
        widgetJSDatePick mbLang
        $(widgetFile "tabulated-form-one-col")

postCercarVolsR :: Handler RepHtml
postCercarVolsR = do
    mbLang <- UF.obtenirIdioma
    paísOrigen <- runInputPost $ ireq textField "f2"
    paísDestí <- runInputPost $ ireq textField "f4"

    destinacionsInicialsOrigen <- runDB $ UH.obtenirDestinacions $ Just paísOrigen
    destinacionsInicialsDestí <- runDB $ UH.obtenirDestinacions $ Just paísDestí
    
    ((res, tabulatedFormWidget), enctype) <- runFormPost $ cercarVolsForm destinacionsInicialsOrigen destinacionsInicialsDestí
    case res of
        FormSuccess (origen, mbDestí, mbData) -> do
                let cercaDestí = case mbDestí of
                       Just destí -> destí
                       Nothing -> "Nothing"
                let cercaData = case fmap toModifiedJulianDay mbData of
                       Just dia -> dia
                       Nothing -> 0

                redirect $ CercarVolsResultR origen cercaDestí cercaData 0
        _ -> do
                setMessageI MsgPleaseCorrect    
                mydefaultLayout emptyWidget $ do
                        toWidgetHead $(juliusFile "templates/uservol-form.julius")
                        widgetJSDatePick mbLang
                        $(widgetFile "tabulated-form-one-col")

diesCercaDates = 15 :: Integer

getCercarVolsResultR :: Text -> Text -> Integer -> Int -> Handler RepHtml
getCercarVolsResultR origen txtMbDestí diaMbData pag = do
    muser <- maybeAuth
    let myOffset = pag * UF.volsEntradesPerPàgina
    
    result <- runDB $ select $ from $ \(av `InnerJoin` ua `InnerJoin` usr) -> do
                on ((ua ^. UserArtIdUsuari) Esql.==. (usr ^. UserId))
                on ((ua ^. UserArtIdArtVol) Esql.==. just (av ^. ArtVolId))
                where_ ((usr ^. UserVeto) Esql.==. val False)
                where_ ((ua ^. UserArtEstat) Esql.==. val EstArt_Exposat)
                where_ (av ^. ArtVolOrigenAeroport Esql.==. val origen)
                when (txtMbDestí /= "Nothing") $ where_ (av ^. ArtVolDestíAeroport Esql.==. val txtMbDestí)
                when (isJust muser) $ where_ (ua ^. UserArtIdUsuari Esql.!=. val (entityKey $ fromJust muser))
                when (diaMbData /= 0) $ where_ ((ua ^. UserArtData Esql.>=. val (limitInf diaMbData)) Esql.&&.
                                                (ua ^. UserArtData Esql.<=. val (limitSup diaMbData)))
                orderBy [desc (ua ^. UserArtPosted)]
                offset $ fromIntegral myOffset
                limit $ fromIntegral UF.volsEntradesPerPàgina
                return (usr, ua, av)

    let result2 = map (\(_,a,b) -> (a,b)) (result::[(Entity User, Entity UserArt, Entity ArtVol)])

    let vols = map userVolFromUserArtEnts result2
    
    let navPrimer = CercarVolsResultR origen txtMbDestí diaMbData 0
        navSegüent = CercarVolsResultR origen txtMbDestí diaMbData (pag +1)
        mbNavAnterior = if pag > 0
                         then Just $ CercarVolsResultR origen txtMbDestí diaMbData (pag -1)
                         else Nothing
                         
    mbUserInfo <- case muser of
                         Nothing -> return Nothing
                         Just (Entity userId _) -> do
                                 mbEnt <- runDB $ getBy (UniqueUserInfoUserId userId)
                                 return $ fmap entityVal mbEnt
                                 
    mbLang <- UF.obtenirIdioma
    
    master <- getYesod
    let destMap = appDestsRevMap master
    
    let ésModificable = False
    mydefaultLayout emptyWidget $ do
        setTitleI MsgFlightSearchTitle
        $(widgetFile "uservol-llistar")
  where
    limitInf dia = addDays (-diesCercaDates) (ModifiedJulianDay dia)
                      
    limitSup dia = addDays diesCercaDates (ModifiedJulianDay dia)
