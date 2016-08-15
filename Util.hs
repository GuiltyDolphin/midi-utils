{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Util
  ( slowTrack
  , speedTrack
  , liftMap
  ) where


import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Data.EventList.Relative.TimeBody as T
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Message.Channel as C


type MIDI = F.T
type Voice = V.T
type MEvent = E.T
type MessageChan = C.T


mapElapsedTimeIntegral :: (Integer -> Integer) -> MIDI -> MIDI
mapElapsedTimeIntegral f = liftMap $ F.toElapsedTime . f . F.fromElapsedTime


slowTrack :: Integer -> MIDI -> MIDI
slowTrack n = mapElapsedTimeIntegral (*n)


speedTrack :: Integer -> MIDI -> MIDI
speedTrack n = mapElapsedTimeIntegral (`div`n)


mapPitch :: (V.Pitch -> V.Pitch) -> Voice -> Voice
mapPitch f (V.NoteOff p v) = V.NoteOff (f p) v
mapPitch f (V.NoteOn  p v) = V.NoteOn  (f p) v
mapPitch f (V.PolyAftertouch p pr) = V.PolyAftertouch (f p) pr
mapPitch _ v = v


mapVelocity :: (V.Velocity -> V.Velocity) -> Voice -> Voice
mapVelocity f (V.NoteOff p v) = V.NoteOff p (f v)
mapVelocity f (V.NoteOn  p v) = V.NoteOn  p (f v)
mapVelocity _ v = v


-- | Instances of @LiftMap a b@ can lift maps from @a -> a@ to @b -> b@.
class LiftMap a b where
  liftMap :: (a -> a) -> b -> b


-- ElapsedTime
instance LiftMap F.ElapsedTime F.Track where
  liftMap = T.mapTime


instance LiftMap F.ElapsedTime MIDI where
  liftMap f = liftMap (liftMap f :: F.Track -> F.Track)


-- MessageChan
instance LiftMap MessageChan MEvent where
  liftMap f (E.MIDIEvent e) = E.MIDIEvent (f e)
  liftMap _ e = e


instance LiftMap MessageChan F.Track where
  liftMap f = liftMap (liftMap f :: MEvent -> MEvent)


instance LiftMap MessageChan MIDI where
  liftMap f = liftMap (liftMap f :: F.Track -> F.Track)


-- MEvent
instance LiftMap MEvent F.Track where
  liftMap = T.mapBody


instance LiftMap MEvent MIDI where
  liftMap f = liftMap (liftMap f :: F.Track -> F.Track)


-- Pitch
instance LiftMap V.Pitch Voice where
  liftMap = mapPitch


instance LiftMap V.Pitch F.Track where
  liftMap f = liftMap (liftMap f :: Voice -> Voice)


instance LiftMap V.Pitch MIDI where
  liftMap f = liftMap (liftMap f :: F.Track -> F.Track)


-- Track
instance LiftMap F.Track MIDI where
  liftMap = F.mapTrack


-- Voice
instance LiftMap Voice MEvent where
  liftMap = E.mapVoice


instance LiftMap Voice F.Track where
  liftMap f = liftMap (liftMap f :: MEvent -> MEvent)


instance LiftMap Voice MIDI where
  liftMap f = liftMap (liftMap f :: F.Track -> F.Track)
