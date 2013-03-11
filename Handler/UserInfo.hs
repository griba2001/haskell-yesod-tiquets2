module Handler.UserInfo where

import Import as I

import Data.Time.Clock (getCurrentTime) -- , UTCTime
import Data.Maybe as MB
import Control.Monad (when)
import qualified Util.UtilForms as UF
import qualified Handler.UtilHandlers as UH
import qualified Util.UtilPaisos as UP

userInfoForm :: UserId -> Maybe UserInfo -> Form UserInfo
userInfoForm idUsuari mbUser = renderTable $ UserInfo
        <$> aformM (return idUsuari)
        <*> aformM (liftIO getCurrentTime)
        <*> (formToAForm $ mreq textField (fieldSettingsLabel MsgUserInfoNom) (userInfoNom <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> (formToAForm $ mreq textField (fieldSettingsLabel MsgUserInfoCognoms) (userInfoCognoms <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> (formToAForm $ mreq textField (fieldSettingsLabel MsgUserInfoAdreça) (userInfoAdreça <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> (formToAForm $ mreq textField (fieldSettingsLabel MsgUserInfoCodiPostal) (userInfoCodiPostal <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> (formToAForm $ mreq textField (fieldSettingsLabel MsgUserInfoPoblació) (userInfoPoblació <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> (formToAForm $ mreq (selectFieldList UP.països) (fieldSettingsLabel MsgUserInfoPaís) (userInfoPaís <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> (formToAForm $ mreq (selectFieldList UF.idiomes) (fieldSettingsLabel MsgUserInfoIdioma) (userInfoIdioma <$> mbUser) >>= (return . fmap UF.vistaAmbEstrellaL))
        <*> aopt textField (fieldSettingsLabel MsgUserInfoTelèfon) (userInfoTelèfon <$> mbUser)
        
userInfoFormCustom :: UserId -> Maybe UserInfo -> Form UserInfo
userInfoFormCustom idUsuari mbUser extra = do -- renderTable $ UserInfo
        -- <$> aformM (return idUsuari)
        currentTime <- liftIO getCurrentTime -- <*> aformM (liftIO getCurrentTime)
        --         (userVolOrigen_Res, userVolOrigen_View) <- mreq (selectFieldList UF.destinacions) (fieldSettingsLabel MsgUserVolOrigen) (userVolOrigen <$> mbUserVol) >>= (return . fmap UF.vistaAmbEstrella)

        (userInfoNom_Res, userInfoNom_View) <- mreq textField (fieldSettingsLabel MsgUserInfoNom) (userInfoNom <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoCognoms_Res, userInfoCognoms_View) <- mreq textField (fieldSettingsLabel MsgUserInfoCognoms) (userInfoCognoms <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoAdreça_Res, userInfoAdreça_View) <- mreq textField (fieldSettingsLabel MsgUserInfoAdreça) (userInfoAdreça <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoCodiPostal_Res, userInfoCodiPostal_View) <- mreq textField (fieldSettingsLabel MsgUserInfoCodiPostal) (userInfoCodiPostal <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoPoblació_Res, userInfoPoblació_View) <- mreq textField (fieldSettingsLabel MsgUserInfoPoblació) (userInfoPoblació <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoPaís_Res, userInfoPaís_View) <- mreq (selectFieldList UP.països) (fieldSettingsLabel MsgUserInfoPaís) (userInfoPaís <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoIdioma_Res, userInfoIdioma_View) <- mreq (selectFieldList UF.idiomes) (fieldSettingsLabel MsgUserInfoIdioma) (userInfoIdioma <$> mbUser) >>= (return . fmap UF.vistaAmbEstrella)
        (userInfoTelèfon_Res, userInfoTelèfon_View) <- mopt textField (fieldSettingsLabel MsgUserInfoTelèfon) (userInfoTelèfon <$> mbUser)

        let userInfo_Res = UserInfo <$> pure idUsuari <*> pure currentTime <*> userInfoNom_Res <*> userInfoCognoms_Res <*>
                                       userInfoAdreça_Res <*> userInfoCodiPostal_Res <*> userInfoPoblació_Res <*> userInfoPaís_Res <*>
                                       userInfoIdioma_Res <*> userInfoTelèfon_Res
                                       
        let wGrups = UH.massRows [(Just MsgUserInfo_GrupNom, [userInfoNom_View, userInfoCognoms_View]),
                                  (Just MsgUserInfo_GrupAdreça, [userInfoAdreça_View, userInfoCodiPostal_View,
                                      userInfoPoblació_View, userInfoPaís_View]),
                                  (Just MsgUserInfo_GrupComm, [userInfoTelèfon_View]),            
                                  (Just MsgUserInfo_GrupAltres, [userInfoIdioma_View])            
                                      ]
        let widget = do
                [whamlet|
        #{extra}
        ^{wGrups}
|]
        return (userInfo_Res, widget)
                                      
        
getUserInfoR :: Handler RepHtml
getUserInfoR = do
    req <- getRequest
    master <- getYesod
    muser <- maybeAuth
    let Entity userId user = fromJust muser  -- isAuthorized
    mbEntUserInfo <- runDB $
                     getBy $ UniqueUserInfoUserId userId -- :: SqlPersist m (Maybe (Entity _ UserInfo))

    (tabulatedFormWidget, enctype) <- generateFormPost $ userInfoFormCustom userId $ fmap entityVal mbEntUserInfo
    mydefaultLayout emptyWidget $ do
        setTitleI MsgUserInfoTitle
        $(widgetFile "tabulated-form-one-col")

postUserInfoR :: Handler RepHtml
postUserInfoR = do
    muser <- maybeAuth
    let Entity userId _user = fromJust muser  -- isAuthorized
    ((res, tabulatedFormWidget), enctype) <- runFormPost $ userInfoFormCustom userId Nothing
    case res of
        FormSuccess userInfo -> do
            runDB $ do
                     deleteBy $ UniqueUserInfoUserId userId
                     insert userInfo >> return ()
                 
            setMessageI MsgUsrInfoHasBeenUpdated
            redirect UserInfoR
            
        _ -> mydefaultLayout emptyWidget $ do
                setTitleI MsgPleaseCorrect
                $(widgetFile "tabulated-form-one-col")


getUserInfoAdminVisitarR :: UserId -> Handler RepHtml
getUserInfoAdminVisitarR userId = do
    mbUser <- runDB $ get userId
    mbEntUserInfo <- runDB $ getBy $ UniqueUserInfoUserId userId
    if (isNothing mbUser || isNothing mbEntUserInfo)
       then notFound
       else do
          let user = fromJust mbUser
              Entity _userInfoId userInfo = fromJust mbEntUserInfo
          mydefaultLayout emptyWidget $ do
                let tabulatedFormWidget = emptyWidget
                    enctype = Multipart
                setTitleI MsgUserInfoAdminViewTitle
                $(widgetFile "userinfo-visitar-camps")
                $(widgetFile "form-canviar-veto")

postUserInfoAdminVisitarR :: UserId -> Handler RepHtml               
postUserInfoAdminVisitarR userId = do
    runDB $ do
        mbUser <- get userId
        let veto = userVeto (fromJust mbUser)
        when (isJust mbUser) $ updateWhere [UserId I.==. userId] [UserVeto I.=. not veto]
        return ()
    redirect (UserInfoAdminVisitarR userId)
        

getUserInfoAdminLlistarArticlesR :: UserId -> Int -> Handler RepHtml
getUserInfoAdminLlistarArticlesR userId pag = do

    mbUserInfo <- runDB $ getBy (UniqueUserInfoUserId userId)
    case mbUserInfo of
      Nothing -> notFound
      Just (Entity _ userInfo) -> do
              
        let navPrimer = UserInfoAdminLlistarArticlesR userId 0
            navSegüent = UserInfoAdminLlistarArticlesR userId (pag +1)
            mbNavAnterior = if pag > 0
                                then Just $ UserInfoAdminLlistarArticlesR userId (pag -1)
                                else Nothing
            myOffset = pag * UF.volsEntradesPerPàgina
                
        mbLang <- UF.obtenirIdioma
        entArts <- runDB $ selectList [UserArtIdUsuari ==. userId] [Desc UserArtData, OffsetBy myOffset, LimitTo UF.volsEntradesPerPàgina]
        let arts = map (\(Entity artId art) -> (artId, art)) entArts
            
        mydefaultLayout emptyWidget $ do
                setTitleI MsgUserInfoAdminLlistarArticlesTitle
                $(widgetFile "userinfo-admin-llistar-arts")
            
