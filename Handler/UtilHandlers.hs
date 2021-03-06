{-# LANGUAGE RankNTypes, ConstraintKinds #-}
module Handler.UtilHandlers (
  icontains,
  massRows, oneRow,
  myTextOptionsPairs, myTextTextOptionsPairs,
  obtenirDestinacions
) where

import Import as I
import Text.Hamlet
-- import Model.UtilAdmins
import Yesod.Default.Config
import Yesod.Widget
import Data.Text as TS
import Data.Tuple as TU
import Data.List as L
import Data.Map as M
import Data.Maybe as MB
import Database.Persist.Store (PersistFilter(BackendSpecificFilter))
import qualified Yesod.Form.Fields as FF
import Database.Esqueleto as Esql
import Database.Persist.GenericSql (SqlPersist)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Control.Monad as CM

icontains :: forall v. EntityField v Text -> Text -> Filter v
icontains field val = Filter field (Left (TS.concat ["%", val, "%"])) (BackendSpecificFilter "ILIKE")

massRows :: [(Maybe AppMessage, [FieldView App App])] -> GWidget App App ()
massRows grups = [whamlet|
$newline never
$forall (mbLegend, views) <- grups
  <tr>
    <td .td-fldset>
      <fieldset>
        $maybe leg <- mbLegend
          <legend>_{leg}
        $nothing
          <legend>. #
        <table .taula-grup>
          <col .form-col-etiq>
          <col .form-col-camp>
          <col .form-col-err>
          <tbody>
            $forall view <- views
                <tr :fvRequired view:.required :not $ fvRequired view:.optional>
                    <td>
                        <label for=#{fvId view}>#{fvLabel view}
                        $maybe tt <- fvTooltip view
                            <div .tooltip>#{tt}
                    <td>^{fvInput view}
                    <td .errors>
                       $maybe err <- fvErrors view
                          #{err}
|]

oneRow :: FieldView App App -> GWidget App App ()
oneRow view = [whamlet|
$newline never
                <tr :fvRequired view:.required :not $ fvRequired view:.optional>
                    <td>
                        <label for=#{fvId view}>#{fvLabel view}
                        $maybe tt <- fvTooltip view
                            <div .tooltip>#{tt}
                    <td>^{fvInput view}
                    <td .errors>
                       $maybe err <- fvErrors view
                          #{err}
|]


{-
renderAppBreadcrumbs :: -> GHandler sub App (GWidget sub app ())
renderAppBreadcrumbs = do
        (actual, bcAncestres) <- breadcrumbs
        return $(widgetFile "breadcrumbs")
        -}


        {-
recentVolsDades :: GHandler sub master (HtmlUrlI18n msg url)
recentVolsDades = do
    ara <- liftIO getCurrentTime
    volsRecents <- runDB $ selectList [UserVolPosted <=. ara] [Desc UserVolPosted, LimitTo 4]
    $(ihamletFile "templates/recent-flights.hamlet")
                -}

myTextOptionsPairs :: (RenderMessage master msg) => [(msg, Text)] -> GHandler sub master (FF.OptionList Text)
myTextOptionsPairs opts = do
  mr <- getMessageRender
  let mkOption (display, internal) =
          Option { optionDisplay       = mr display
                 , optionInternalValue = internal
                 , optionExternalValue = internal
                 }
  return $ FF.mkOptionList (I.map mkOption opts)

myTextTextOptionsPairs :: [(Text, Text)] -> GHandler sub master (FF.OptionList Text)
myTextTextOptionsPairs opts = do
  -- mr <- getMessageRender
  let mkOption (display, internal) =
          Option { optionDisplay       = display
                 , optionInternalValue = internal
                 , optionExternalValue = internal
                 }
  return $ FF.mkOptionList (I.map mkOption opts)

obtenirDestinacions :: (PersistQuery SqlPersist m, MonadLogger m, MonadResourceBase m) => Maybe Text -> SqlPersist m [(Text, Text)]
obtenirDestinacions mbCodiPaís = do
    result <- select $ from $ \(pa `InnerJoin` ae) -> do
                on ((pa ^. PaísNom) Esql.==. (ae ^. AeroportPaís))
                when (MB.isJust mbCodiPaís) $ where_ (pa ^. PaísCodi Esql.==. val (fromJust mbCodiPaís))
                orderBy [asc (ae ^. AeroportPoblació)]
                return (pa, ae)

    return $ L.map (fn . entityVal . snd) (result :: [(Entity País, Entity Aeroport)] )
  where
    fn aero = (primer aero, aeroportCodi aero)
    primer aero = let mbNom = aeroportNom aero
                  in case mbNom of
                          Just nom -> TS.unwords [aeroportPoblació aero, nom, parentesi $ aeroportCodi aero]
                          Nothing -> TS.unwords [aeroportPoblació aero, parentesi $ aeroportCodi aero]
    parentesi text = "(" `TS.append` text `TS.append` ")"
    


                