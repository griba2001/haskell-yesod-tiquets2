module Util.MissatgesAuth where

-- importat a Foundation

import Yesod.Auth.Message
import Data.Monoid (mappend)
import Data.Text (Text)

catalanAuthMessage :: AuthMessage -> Text
catalanAuthMessage NoOpenID = "No s'ha trobat cap identificador OpenID"
catalanAuthMessage LoginOpenID = "Inici de sessió via OpenID"
catalanAuthMessage LoginGoogle = "Inici de sessió via Google"
catalanAuthMessage LoginYahoo = "Inici de sessió via Yahoo"
catalanAuthMessage Email = "Adreça electrònica"
catalanAuthMessage Password = "Contrasenya"
catalanAuthMessage Register = "Inscriure's"
catalanAuthMessage RegisterLong = "Crear un compte"
catalanAuthMessage EnterEmail = "Entreu la vostra adreça electrònica, i us hi enviarem un missatge de confirmació."
catalanAuthMessage ConfirmationEmailSentTitle = "S'ha enviat el missatge de confirmació"
catalanAuthMessage (ConfirmationEmailSent email) =
    "Un missatge electrònic de confirmació us ha estat enviat" `mappend`
    email `mappend`
    "."
catalanAuthMessage AddressVerified = "La vostra adreça electrònica ha estat validada, si us plau establiu la nova contrasenya"
catalanAuthMessage InvalidKeyTitle = "Clau de verificació incorrecta"
catalanAuthMessage InvalidKey = "Ho sentim però la clau de verificació és incorrecta."
catalanAuthMessage InvalidEmailPass = "La combinació adreça electrònica/contrasenya no és vàlida"
catalanAuthMessage BadSetPass = "Cal iniciar sessió per poder establir una contrasenya"
catalanAuthMessage SetPassTitle = "Canviar la contrasenya"
catalanAuthMessage SetPass = "Escollir la contrasenya"
catalanAuthMessage NewPass = "Nova contrasenya"
catalanAuthMessage ConfirmPass = "Confirmar contrasenya"
catalanAuthMessage PassMismatch = "Les contrasenyes no casen, torneu-hi si us plau"
catalanAuthMessage PassUpdated = "Contrasenya actualitzada"
catalanAuthMessage Facebook = "Inici de sessió via Facebook"
catalanAuthMessage LoginViaEmail = "Inici de sessió via correu electrònic"
catalanAuthMessage InvalidLogin = "L'inici de sessió no és vàlid"
catalanAuthMessage NowLoggedIn = "Ara ja heu iniciat la sessió"
catalanAuthMessage LoginTitle = "Inici de sessió"
catalanAuthMessage PleaseProvideUsername = "Si us plau, ompliu el nom d'usuari"
catalanAuthMessage PleaseProvidePassword = "Si us plau, ompliu la contrasenya"

