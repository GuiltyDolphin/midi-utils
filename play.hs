module Play where

import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.IO as SoundIO
import Sound.MIDI.File (T)

import qualified System.IO as SIO
import qualified System.Directory as SD

import System.Process

import qualified Text.Parsec as Ps
import qualified Text.Parsec.Token as Ps
import Data.Function (on)


newtype Port   = Port { getPort :: (Int, Int) }
  deriving (Eq)


instance Show Port where
  show (Port (c, p)) = concat [show c, ":", show p]


data User = User { userPort :: Port
                    , userClientName :: String
                    , userPortName :: String
                    } deriving (Show, Eq)



data PlayMidi = PlayMidi { midiPort :: Port, midiFile :: FilePath }


toProcess :: PlayMidi -> CreateProcess
toProcess (PlayMidi { midiPort = p, midiFile = f }) = shell $ concat ["aplaymidi", " -p ", show p, " ", f]


runMidi :: Port -> T -> IO ()
runMidi p m = do
  SD.createDirectoryIfMissing True "tmp-midi"
  (tmp, w) <- SIO.openBinaryTempFile "tmp-midi" "tmpXXX.midi"
  SIO.hClose w
  writeMidi tmp
  createProcess (midiProc tmp)
  return ()
  where writeMidi f = Save.toFile f m
        midiProc f = toProcess (PlayMidi p f)


availableUsers :: IO (Either String [User])
availableUsers = (checkParse <$> runParse <$> usersRaw)
  where usersRaw = readProcess "aplaymidi" ["-l"] ""
        parsePort = ((curry Port `on` fromInteger) <$> decimal <*> (Ps.char ':' *> decimal))
        parseCname = Ps.manyTill Ps.anyChar (Ps.try (Ps.space *> Ps.space))
        parsePname = Ps.manyTill Ps.anyChar (Ps.try Ps.newline)
        parseUser = User
                    <$> Ps.between Ps.spaces Ps.spaces parsePort
                    <*> (parseCname <* Ps.spaces) <*> parsePname
        parseUsers = Ps.manyTill Ps.anyChar Ps.newline *> Ps.many parseUser
        runParse = Ps.parse parseUsers ""
        checkParse (Left _) = Left "Failed to parse users"
        checkParse (Right r) = Right r
        decimal = read <$> Ps.many1 Ps.digit
