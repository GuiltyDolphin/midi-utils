{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sound.MIDI.Utils.Player
  ( Player(..)
  , PlayerStream(..)
  , PlayerFile(..)
  ) where


import qualified Sound.MIDI.File as F


type MIDI = F.T


-- | Players that can play MIDI on a port.
class Player player port | player -> port where
  -- | Use a port to construct a @player@.
  makePlayer :: port -> player


-- | Players that can directly play MIDI data.
class PlayerStream player where
  -- | Play the given 'MIDI' data to the port.
  playMidi :: player -> MIDI -> IO ()


-- | Players that can directly play a MIDI file.
class PlayerFile player where
  -- | Play a 'MIDI' file to the port.
  playMidiFile :: player -> FilePath -> IO ()
