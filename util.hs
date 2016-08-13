module Util where

import qualified Sound.MIDI.File as F
import qualified Data.EventList.Relative.TimeBody as T

mapElapsedTime :: (Integer -> Integer) -> F.T -> F.T
mapElapsedTime f = F.mapTrack (T.mapTime (F.toElapsedTime . f . F.fromElapsedTime))

slowTrack :: Integer -> F.T -> F.T
slowTrack n = mapElapsedTime (*n)

speedTrack :: Integer -> F.T -> F.T
speedTrack n = mapElapsedTime (`div`n)
