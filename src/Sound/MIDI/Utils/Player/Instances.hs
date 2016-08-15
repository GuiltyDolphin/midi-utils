{-# LANGUAGE MultiParamTypeClasses #-}
module Sound.MIDI.Utils.Player.Instances
  ( ALSAPort
  , APlayMidi
  , PMidi
  ) where


import Data.Monoid ((<>))
import System.Process
import qualified System.IO as SIO
import qualified Data.ByteString as BS

import qualified Sound.MIDI.File.Save as Save

import Sound.MIDI.Utils.Player (Player(..))


type Port = (Int, Int)


newtype ALSAPort = ALSAPort { getALSAPort :: Port }


instance Show ALSAPort where
  show (ALSAPort (x,y)) = show x <> ":" <> show y


data PlayerTemplate port = PT { playerPort :: port
                              , playerFile :: FilePath
                              , playerName :: String
                              }


newtype APlayMidi = APlayMidi { getAPlayMidi :: PlayerTemplate ALSAPort }


newtype PMidi = PMidi { getPMidi :: PlayerTemplate ALSAPort }


instance Player APlayMidi ALSAPort where
  makePlayer p = APlayMidi (PT { playerPort = p, playerFile = "-", playerName = "aplaymidi" })
  playMidi (APlayMidi p) = defaultAlsaPlayMidi p


instance Player PMidi ALSAPort where
  makePlayer p = PMidi (PT { playerPort = p, playerFile = "-", playerName = "pmidi" })
  playMidi (PMidi p) = defaultAlsaPlayMidi p


toProcess :: (Show p) => PlayerTemplate p -> CreateProcess
toProcess (PT { playerPort = p, playerFile = f, playerName = n }) = shell $ concat [n, " -p ", show p, " ", f]


defaultAlsaPlayMidi p m = do
  (b1,b2) <- createPipe
  SIO.hSetBinaryMode b2 True
  BS.hPutStr b2 midiBS
  createProcess (midiProc { std_in=UseHandle b1 })
  pure ()
  where midiBS = BS.pack $ Save.toByteList m
        midiProc = toProcess p
