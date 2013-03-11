module Handler.HXR_Aeroports where

import Import as I
import "xmlgen" Text.XML.Generator
import Util.UtilForms as UF
import Data.Text as T
import Blaze.ByteString.Builder (Builder)
import Handler.UtilHandlers as UH

tagDest = "dest" :: String
tagDests = "destinacions" :: String
litCodi = "codi" :: String

getHXR_AeroportsR :: Text -> Handler RepXml
getHXR_AeroportsR país = do
    -- let mbDests = UF.destinacionsDelPaís país
    lDests <- runDB $ UH.obtenirDestinacions $ Just país
    return $ RepXml $ toContent $ content lDests
  where
    content :: [(Text, Text)] -> Builder
    content lDests = case lDests of
                   
        [] -> xrender $ doc defaultDocInfo $ (xelem tagDests) noElems
        
        dests -> let ch = xelems $ I.map myxmlfn dests
                      in xrender $ doc defaultDocInfo $ (xelem tagDests) ch
                      
        
    myxmlfn :: (Text, Text) -> Xml Elem    
    myxmlfn (nom, codi) = xelem tagDest (xattr litCodi $ T.unpack codi, xtext $ T.unpack nom)