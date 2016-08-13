module Play where

import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.IO as SoundIO
import Sound.MIDI.File (T)

import qualified System.IO as SIO
import qualified System.Directory as SD

import System.Process

type Client = Int
type Port   = Int

newtype User = User { getUser :: (Client, Port) }

instance Show User where
  show (User (c, p)) = concat [show c, ":", show p]


data PlayMidi = PlayMidi { midiUser :: User, midiFile :: FilePath }

toProcess :: PlayMidi -> CreateProcess
toProcess (PlayMidi { midiUser = u, midiFile = f }) = shell $ concat ["aplaymidi", " -p ", show u, " ", f]


runMidi :: User -> T -> IO ()
runMidi u m = do
  SD.createDirectoryIfMissing True "tmp-midi"
  (tmp, w) <- SIO.openBinaryTempFile "tmp-midi" "tmpXXX.midi"
  SIO.hClose w
  writeMidi tmp
  createProcess (midiProc tmp)
  return ()
  where writeMidi f = Save.toFile f m
        midiProc f = toProcess (PlayMidi u f)
