module Util.UtilForms where
{- (
  obtenirIdioma,
  vistaAmbEstrella,
  countries, nomPaís,
  destinacions, nomDest,
  monedes, nomMoneda, nomMonedaCurt,
  idiomes, nomIdioma,
  today,
  volsEntradesPerPàgina,
  correuEntradesPerPàgina,
  usuarisEntradesPerPàgina,
  formataDay, formataUTCT, formataHoraLlistatCorreu, formataDayCurt,
  showPreu, showBoolYesNo,
  mbTextToText, txtToMbText,
  nomTipusArticle, preuDeVenda
  )
-}

import Import
import Data.Map as M
import Data.Tuple as TU
import Data.Maybe as MB
import Data.List as L
import qualified Data.Text as TS
-- import Yesod.Form.Types
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime) -- , UTCTime
-- import Text.Printf (printf)
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (formatTime)
import System.Locale (TimeLocale(..), defaultTimeLocale, iso8601DateFormat)
import Numeric
import qualified Data.Text as TS


volsEntradesPerPàgina = 10 :: Int
correuEntradesPerPàgina = 10 :: Int
usuarisEntradesPerPàgina = 10 :: Int

{-
countries :: [(AppMessage, Text)]
countries = [(MsgCtrySpain,"ES"),(MsgCtryUK,"UK"),(MsgCtryFrance,"FR"), (MsgCtryGermany,"DE")]


nomPaís :: Text -> AppMessage
nomPaís codi = case M.lookup codi countriesRevMap of
                     Just nom -> nom
                     Nothing -> MsgUnknown

countriesRevMap :: Map Text AppMessage
countriesRevMap = M.fromList $ L.map TU.swap countries
-}

--

idiomes :: [(AppMessage, Text)]
idiomes = [(MsgIdiomaEN, "en"), (MsgIdiomaCA, "ca")]


nomIdioma :: Text -> AppMessage
nomIdioma codi = case M.lookup codi idiomesRevMap of
                     Just nom -> nom
                     Nothing -> MsgUnknown

idiomesRevMap :: Map Text AppMessage
idiomesRevMap = M.fromList $ L.map TU.swap idiomes
                     
--
païsosIDestinacions :: [(Text, [(Text, Text)])]
païsosIDestinacions = [("ES",[("Barcelona (BCN)", "BCN"), ("Madrid (MAD)", "MAD")]),
                       ("GB",[("London Gatwick (GTW)", "GTW"), ("Bristol Airport (BRS)","BRS")]),
                       ("FR",[("Paris Charles De Gaulle (CDG)", "CDG"), ("Marseille Provence Airport (MRS)","MRS")]),
                       ("DE",[("Frankfurt Airport (FRA)","FRA"), ("Berlin Brandenburg Airport (BER)","BER")])
                       ]
                       
nomDest :: Map Text Text -> Text -> Text
nomDest destMap dest = do
        case M.lookup dest destMap of
                        Just nom -> nom
                        Nothing -> dest

destinacionsRevMap :: [(Text, Text)] -> Map Text Text
destinacionsRevMap destinacions = M.fromList $ L.map TU.swap destinacions

{-

destinacionsDelPaís :: Text -> Maybe [(Text, Text)]
destinacionsDelPaís país = fmap (M.assocs . M.fromList) $ M.lookup país $ M.fromList païsosIDestinacions
-}

--

monedes :: [(Text, Moneda)]
monedes = [("US Dollar (USD)",USD), ("Euro (EUR)",EUR), ("UK Pound (GBP)",GBP), ("Swiss franc (CHF)",CHF)]

nomMoneda :: Moneda -> Text
nomMoneda mon = case M.lookup mon monedesRevMap of
                     Just nom -> nom
                     Nothing -> "unknown"

nomMonedaCurt :: Moneda -> String
nomMonedaCurt mon = show mon
                     
monedesRevMap :: Map Moneda Text
monedesRevMap = M.fromList $ L.map TU.swap monedes
                     
--

showPreu :: Double -> String
showPreu preu = showGFloat (Just 2) preu ""

showBoolYesNo :: Bool -> AppMessage
showBoolYesNo b = if b then MsgYes else MsgNo

vistaAmbEstrella :: FieldView sub master -> FieldView sub master
vistaAmbEstrella fv @ FieldView {..} = fv {fvLabel = mappend fvLabel " *"}

vistaAmbEstrellaL :: FieldView sub master -> [FieldView sub master]
vistaAmbEstrellaL fv @ FieldView {..} = [fv {fvLabel = mappend fvLabel " *"}]

today :: () -> IO Day
today () = do
        fmap utctDay getCurrentTime

{-
obtenirIdioma :: Maybe UserInfo -> Handler (Maybe Text)
obtenirIdioma mbUserInfo = do
        case mbUserInfo of
             Just UserInfo {..} -> return $ Just userInfoIdioma
             Nothing -> do
                     mbSessLang <- lookupSession "_LANG"
                     case mbSessLang of
                          Just _ -> return mbSessLang
                          Nothing -> do
                            langs <- languages
                            return $ MB.listToMaybe $ L.filter (`elem` ["ca","en"]) langs
                            -}

-- escurça a dos caràcters (en_GB a en) i filtra els suportats
obtenirIdioma :: GHandler s m (Maybe Text)
obtenirIdioma = languages >>= (return . L.map (TS.take 2)) >>= (return . MB.listToMaybe . L.filter (`elem` idiomesSuportats))

formataDay :: Maybe Text -> Day -> String
formataDay mbLang dia =
        case mbLang of
          Nothing -> formatTime defaultTimeLocale englishFormat dia  -- (iso8601DateFormat Nothing)
          Just lang -> case lang of
                                     "ca" -> formatTime catalanLocale catFormat dia
                                     _ -> formatTime defaultTimeLocale englishFormat dia
   where
           catFormat = "%a., %d de %b. de %Y"
           englishFormat = "%a., %b. %d, %Y"

formataDayCurt :: Maybe Text -> Day -> String
formataDayCurt mbLang dia =
        case mbLang of
          Nothing -> formatTime defaultTimeLocale englishFormat dia  -- (iso8601DateFormat Nothing)
          Just lang -> case lang of
                                     "ca" -> formatTime catalanLocale catFormat dia
                                     _ -> formatTime defaultTimeLocale englishFormat dia
   where
           catFormat = "%Y-%b-%d"
           englishFormat = "%Y-%b-%d"
{-           
           catFormat = "%d/%b/%Y"
           englishFormat = "%b/%d/%Y"
                   -}
                   
formataUTCT :: Maybe Text -> UTCTime -> String
formataUTCT mbLang moment =
        case mbLang of
          Nothing -> formatTime defaultTimeLocale englishFormat moment  -- (iso8601DateFormat Nothing)
          Just lang -> case lang of
                                     "ca" -> formatTime catalanLocale catFormat moment
                                     _ -> formatTime defaultTimeLocale englishFormat moment
   where
           catFormat = "%H:%M %a. %d de %b. de %Y"
           englishFormat = "%H:%M %a. %b. %d, %Y"
        
formataHoraLlistatCorreu :: Maybe Text -> UTCTime -> String
formataHoraLlistatCorreu mbLang moment =
        case mbLang of
          Nothing -> formatTime defaultTimeLocale englishFormat moment  -- (iso8601DateFormat Nothing)
          Just lang -> case lang of
                                     "ca" -> formatTime catalanLocale catFormat moment
                                     _ -> formatTime defaultTimeLocale englishFormat moment
   where
           catFormat = "%x %T"
           englishFormat = "%x %T"

catalanLocale :: TimeLocale
catalanLocale = defaultTimeLocale { months = [("Gener","Gen"),("Febrer","Feb"),("Març","Mar"),("Abril","Abr"),("Maig","Mai"),("Juny","Jun"),
                                              ("Juliol","Jul"),("Agost","Ago"),("Setembre","Set"),("Octubre","Oct"),("Novembre","Nov"),("Desembre","Des")],
                                    wDays = [("Diumenge","Dg"),("Dilluns","Dl"),("Dimarts","Dt"),("Dimecres","Dm"),
                                            ("Dijous","Dj"),("Divendres","Dv"),("Dissabte","Ds")],
                                    dateFmt = "%d/%m/%Y",
                                    timeFmt = "%H:%M:%S",
                                    dateTimeFmt = " %a %e %b %Y %H:%M:%S"
                                    }
                                              

txtToMbText :: Text -> Maybe Text
txtToMbText txt = case txt of
                          "Nothing" -> Nothing
                          _ -> Just txt

mbTextToText :: Maybe Text -> Text
mbTextToText mbText = case mbText of
                           Nothing -> "Nothing"
                           Just txt -> txt

tipusArticles :: [(AppMessage, TipusArticle)]
tipusArticles = [(MsgTipArtVol, TipArtVol), (MsgTipArtAllotjament, TipArtAllotjament)]

tipusArticlesRevMap :: Map TipusArticle AppMessage
tipusArticlesRevMap = M.fromList $ L.map TU.swap tipusArticles

nomTipusArticle :: TipusArticle -> AppMessage
nomTipusArticle codi = case M.lookup codi tipusArticlesRevMap of
                     Just nom -> nom
                     Nothing -> MsgUnknown

preuDeVenda :: DadesGlobals -> Double -> Double
preuDeVenda dg preu = preu * (1.0 + dgComissió dg)

estatsArticle :: [(AppMessage, EstatArt)]
estatsArticle = [(MsgEstArt_Exposat, EstArt_Exposat), (MsgEstArt_Anuŀlat, EstArt_Anuŀlat),
                 (MsgEstArt_Compromès, EstArt_Compromès), (MsgEstArt_Venut, EstArt_Venut)]
                 

estatsArticleRevMap :: Map EstatArt AppMessage
estatsArticleRevMap = M.fromList $ L.map TU.swap estatsArticle

nomEstatsArticle :: EstatArt -> AppMessage
nomEstatsArticle codi = case M.lookup codi estatsArticleRevMap of
                     Just nom -> nom
                     Nothing -> MsgUnknown

andAlso :: Bool -> Bool -> Bool
andAlso = (&&)

escapeXmlChars :: TS.Text -> TS.Text
escapeXmlChars txt = TS.concatMap fn txt
  where
        fn :: Char -> TS.Text  
        fn '&'  =   "&amp;"
        fn '\"' =  "&quot;"
        fn '\'' =  "&apos;"
        fn '<'  =   "&lt;"
        fn '>'  =   "&gt;"
        fn ch   = TS.singleton ch

parseHelper :: (Monad m) --, RenderMessage master FormMessage
            => (Text -> Either FormMessage a)
            -> [Text] ->  m (Either (SomeMessage App) (Maybe a))
            
parseHelper _ [] = return $ Right Nothing
parseHelper _ ("":_) = return $ Right Nothing
parseHelper _ ("none":_) = return $ Right Nothing
parseHelper f (x:_) = return $ either (Left . SomeMessage) (Right . Just) $ f x