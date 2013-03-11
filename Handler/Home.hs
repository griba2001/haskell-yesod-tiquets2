{-# LANGUAGE TupleSections #-}
module Handler.Home where

import Import
import qualified "mdqq" Text.Shakespeare.LightMarkup as LM
import qualified Handler.UtilHandlers as UH
import qualified Handler.UserVolList as UV
import qualified Util.UtilForms as UF

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    muser <- maybeAuth
    mbLang <- UF.obtenirIdioma
    volsRecents <- UV.obtenirVolsRecents
    master <- getYesod
    let destMap = appDestsRevMap master
    
    mydefaultLayout (UV.widgetVolsRecents destMap muser mbLang volsRecents) $ do
        aDomId <- lift newIdent
        setTitle "Welcome!"
        $(widgetFile "homepage")
        toWidget $ LM.markdownToHtmlUrl $(LM.lmFile "templates/novetats.markdown")

getCreditsR :: Handler RepHtml
getCreditsR = do
    muser <- maybeAuth
    mbLang <- UF.obtenirIdioma
    volsRecents <- UV.obtenirVolsRecents
    master <- getYesod
    let destMap = appDestsRevMap master
    
    mydefaultLayout (UV.widgetVolsRecents destMap muser mbLang volsRecents) $ do
        setTitle "Credits"
        toWidget $ LM.markdownToHtmlUrl $(LM.lmFile "templates/credits.markdown")

{-
sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
    -}