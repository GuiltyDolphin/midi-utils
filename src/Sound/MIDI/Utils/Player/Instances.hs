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

import Sound.MIDI.Utils.Player ( Player(..)
                               , PlayerStream(..)
                               , PlayerFile(..))


type Port = (Int, Int)


newtype ALSAPort = ALSAPort { getALSAPort :: Port }


instance Show ALSAPort where
  show (ALSAPort (x,y)) = show x <> ":" <> show y


data PlayerTemplate port = PT { playerPort :: port
                              , playerName :: String
                              }


newtype APlayMidi = APlayMidi { getAPlayMidi :: PlayerTemplate ALSAPort }


newtype PMidi = PMidi { getPMidi :: PlayerTemplate ALSAPort }


instance Player APlayMidi ALSAPort where
  makePlayer p = APlayMidi (PT { playerPort = p, playerName = "aplaymidi" })


instance Player PMidi ALSAPort where
  makePlayer p = PMidi (PT { playerPort = p, playerName = "pmidi" })


instance PlayerStream APlayMidi where
  playMidi (APlayMidi p) = defaultAlsaPlayMidi p


instance PlayerStream PMidi where
  playMidi (PMidi p) = defaultAlsaPlayMidi p


instance PlayerFile APlayMidi where
  playMidiFile (APlayMidi p) = defaultAlsaPlayMidiFile p


instance PlayerFile PMidi where
  playMidiFile (PMidi p) = defaultAlsaPlayMidiFile p


toProcess :: (Show p) => PlayerTemplate p -> FilePath -> CreateProcess
toProcess (PT { playerPort = p, playerName = n }) f = shell $ concat [n, " -p ", show p, " ", f]


defaultAlsaPlayMidi p m = do
  (b1,b2) <- createPipe
  SIO.hSetBinaryMode b2 True
  BS.hPutStr b2 midiBS
  createProcess (midiProc { std_in=UseHandle b1 })
  pure ()
  where midiBS = BS.pack $ Save.toByteList m
        midiProc = toProcess p "-"


defaultAlsaPlayMidiFile p f = createProcess (toProcess p f) >> pure ()
