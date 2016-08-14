module Util
  ( mapElapsedTime
  , slowTrack
  , speedTrack
  ) where


import qualified Sound.MIDI.File as F
import qualified Data.EventList.Relative.TimeBody as T
import qualified Sound.MIDI.Message.Channel.Voice as V


type MIDI = F.T


type Voice = V.T


mapElapsedTime :: (Integer -> Integer) -> MIDI -> MIDI
mapElapsedTime f = F.mapTrack (T.mapTime (F.toElapsedTime . f . F.fromElapsedTime))


slowTrack :: Integer -> MIDI -> MIDI
slowTrack n = mapElapsedTime (*n)


speedTrack :: Integer -> MIDI -> MIDI
speedTrack n = mapElapsedTime (`div`n)


mapPitch :: (V.Pitch -> V.Pitch) -> Voice -> Voice
mapPitch f (V.NoteOff p v) = V.NoteOff (f p) v
mapPitch f (V.NoteOn  p v) = V.NoteOn  (f p) v
mapPitch f (V.PolyAftertouch p pr) = V.PolyAftertouch (f p) pr
mapPitch _ v = v


mapVelocity :: (V.Velocity -> V.Velocity) -> Voice -> Voice
mapVelocity f (V.NoteOff p v) = V.NoteOff p (f v)
mapVelocity f (V.NoteOn  p v) = V.NoteOn  p (f v)
mapVelocity _ v = v
