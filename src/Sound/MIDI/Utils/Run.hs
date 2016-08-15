module Sound.MIDI.Utils.Run
  ( playMidi
  , playMidiFile
  , listAvailableUsers
  , User(userPort, userClientName, userPortName)
  , Port, mkPort
  ) where


import Control.Monad ((>=>))
import Data.Function (on)
import System.Process
import qualified System.IO as SIO
import qualified Data.ByteString as BS

import qualified Text.Parsec as Ps
import qualified Text.Parsec.Token as Ps

import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Load as Load
import Sound.MIDI.File (T)


newtype Port = Port { getPort :: (Int, Int) }
  deriving (Eq)


mkPort :: (Int, Int) -> Port
mkPort = Port


instance Show Port where
  show (Port (c, p)) = concat [show c, ":", show p]


data User = User { userPort :: Port
                 , userClientName :: String
                 , userPortName :: String
                 } deriving (Show, Eq)


data PlayMidi = PlayMidi { midiPort :: Port, midiFile :: FilePath }


toProcess :: PlayMidi -> CreateProcess
toProcess (PlayMidi { midiPort = p, midiFile = f }) = shell $ concat ["aplaymidi", " -p ", show p, " ", f]


-- | Play MIDI data to the specified ALSA sequencer port.
playMidi :: Port -> T -> IO ()
playMidi p m = do
  (b1,b2) <- createPipe
  SIO.hSetBinaryMode b2 True
  BS.hPutStr b2 midiBS
  createProcess ((midiProc "-") { std_in=UseHandle b1 })
  pure ()
  where midiBS = BS.pack $ Save.toByteList m
        midiProc f = toProcess (PlayMidi p f)


playMidiFile :: Port -> FilePath -> IO ()
playMidiFile p = Load.fromFile >=> playMidi p


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


listAvailableUsers :: IO [User]
listAvailableUsers = either (const []) id <$> availableUsers
