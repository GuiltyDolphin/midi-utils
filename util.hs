module Util
  ( mapElapsedTime
  , slowTrack
  , speedTrack
  ) where


import qualified Sound.MIDI.File as F
import qualified Data.EventList.Relative.TimeBody as T


type MIDI = F.T


mapElapsedTime :: (Integer -> Integer) -> MIDI -> MIDI
mapElapsedTime f = F.mapTrack (T.mapTime (F.toElapsedTime . f . F.fromElapsedTime))


slowTrack :: Integer -> MIDI -> MIDI
slowTrack n = mapElapsedTime (*n)


speedTrack :: Integer -> MIDI -> MIDI
speedTrack n = mapElapsedTime (`div`n)
