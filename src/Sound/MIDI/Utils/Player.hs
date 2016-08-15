{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sound.MIDI.Utils.Player
  ( Player(..)
  ) where


import qualified Sound.MIDI.File as F


type MIDI = F.T


class Player player port | player -> port where
  makePlayer :: port -> player
  playMidi   :: player -> MIDI -> IO ()
