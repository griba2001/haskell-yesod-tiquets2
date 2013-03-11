{-# LANGUAGE ConstraintKinds #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as TS
import Database.Persist.Quasi
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Calendar (Day)
-- import Data.Int (Int64)
import Database.Persist.GenericSql (SqlPersist)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)

data TipusArticle = TipArtVol | TipArtAllotjament deriving (Eq, Show, Read, Ord, Enum)
derivePersistField "TipusArticle"

data Moneda = USD | GBP | EUR | CHF deriving (Eq, Show, Read, Ord, Enum)
derivePersistField "Moneda"

data EstatArt = EstArt_Exposat | EstArt_Anuŀlat | EstArt_Compromès | EstArt_Venut deriving (Eq, Show, Read, Ord, Enum)
derivePersistField "EstatArt"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data DadesGlobals = DadesGlobals {dgComissió::Double, dgAdmins::[Text]}

llegirGlobals :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => SqlPersist m (Maybe DadesGlobals)
llegirGlobals = do
        mbGlobals <- getBy $ UniqueGlobalsIndex 1
        case mbGlobals of
             Just (Entity _ v) -> return $ Just DadesGlobals {dgComissió = globalsComissió v, dgAdmins = (TS.splitOn "," $ globalsAdminList v)}
             Nothing -> do
                     currentTime <- liftIO getCurrentTime
                     let globals = Globals {globalsIndex = 1, globalsPosted=currentTime, globalsComissió = defComissió, globalsAdminList = defAdmin}
                     _ <- insert globals
                     return $ Just DadesGlobals {dgComissió = defComissió, dgAdmins = [defAdmin]}
  where
    defComissió = 0.10
    defAdmin = "griba2001@gmail.com"

userÉsAdmin :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => Maybe (Entity User) -> SqlPersist m Bool
userÉsAdmin (Just (Entity _ user)) = do
        mbDG <- llegirGlobals
        case mbDG of
             Nothing -> return False
             Just dg -> return $ userEmail user `elem` dgAdmins dg

userÉsAdmin _ = return False                     

emptyWidget :: GWidget sub master ()
emptyWidget = (toWidget . toHtml) (""::String)

