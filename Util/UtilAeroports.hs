module Util.UtilAeroports where

import Prelude
import Data.Text as T
import Data.Tuple (swap)
import Data.List as L
import Data.Char as Char
import Data.Map as M
import Text.Printf (printf)
import Control.Monad

x |> f = f x

aeroports :: String -> [(Text, Text, Text, Text, Text)]
aeroports contingut = L.lines contingut
           |> L.tail
           |> L.map (processa . T.init . T.pack)


processa :: Text -> (Text, Text, Text, Text, Text)
processa lin = fn $ T.split (==',') lin


fn :: [Text] -> (Text, Text, Text, Text, Text)
fn c = (codi, poble, provincia, aeroport, país)
   where
        codi = st(c!!0)
        poble = st(c!!1)
        provincia = st(c!!2)
        aeroport = st(c!!3)
        país = st(c!!4)

st x = let y = T.strip x
       in case T.uncons y of
            Just ('"',r1) -> case T.uncons r1 of
                               Just ('[',r2) -> T.init $ T.init r2
                               Just ('\xA0',_) -> ""
                               Just ( _,r2) -> if T.last r1 == '"' then T.init r1 else r1
                               _ -> y
            _ -> y
