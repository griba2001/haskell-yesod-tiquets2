module Handler.SetLang where

import Import

-- import Data.Time.Clock (getCurrentTime) -- , UTCTime
import Data.Maybe
-- import qualified Util.UtilForms as UF

getSetLangR :: Handler RepHtml
getSetLangR = do
    mbIdioma <- lookupGetParams "lang"
    mbReferer <- lookupGetParams "ref"
    case mbIdioma of
      [idioma] -> setLanguage idioma
      _ -> return ()
    case mbReferer of
      [ref] -> redirect ref   
      _ -> redirect HomeR
