{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIPlayer
--
-- A player for music file formats (MIDI, iMelody).
--
-- Generated bindings for @AVMIDIPlayer@.
module ObjC.AVFAudio.AVMIDIPlayer
  ( AVMIDIPlayer
  , IsAVMIDIPlayer(..)
  , initWithContentsOfURL_soundBankURL_error
  , initWithData_soundBankURL_error
  , prepareToPlay
  , play
  , stop
  , duration
  , playing
  , rate
  , setRate
  , currentPosition
  , setCurrentPosition
  , initWithContentsOfURL_soundBankURL_errorSelector
  , initWithData_soundBankURL_errorSelector
  , prepareToPlaySelector
  , playSelector
  , stopSelector
  , durationSelector
  , playingSelector
  , rateSelector
  , setRateSelector
  , currentPositionSelector
  , setCurrentPositionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithContentsOfURL:soundBankURL:error:
--
-- Create a player with the contents of the file specified by the URL.
--
-- 'bankURL' should contain the path to a SoundFont2 or DLS bank to be used 		by the MIDI synthesizer.  For OSX it can be set to nil for the default, 		but for iOS it must always refer to a valid bank file.
--
-- ObjC selector: @- initWithContentsOfURL:soundBankURL:error:@
initWithContentsOfURL_soundBankURL_error :: (IsAVMIDIPlayer avmidiPlayer, IsNSURL inURL, IsNSURL bankURL, IsNSError outError) => avmidiPlayer -> inURL -> bankURL -> outError -> IO (Id AVMIDIPlayer)
initWithContentsOfURL_soundBankURL_error avmidiPlayer  inURL bankURL outError =
withObjCPtr inURL $ \raw_inURL ->
  withObjCPtr bankURL $ \raw_bankURL ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avmidiPlayer (mkSelector "initWithContentsOfURL:soundBankURL:error:") (retPtr retVoid) [argPtr (castPtr raw_inURL :: Ptr ()), argPtr (castPtr raw_bankURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithData:soundBankURL:error:
--
-- Create a player with the contents of the data object
--
-- 'bankURL' should contain the path to a SoundFont2 or DLS bank to be used		by the MIDI synthesizer.  For OSX it can be set to nil for the default,		but for iOS it must always refer to a valid bank file.
--
-- ObjC selector: @- initWithData:soundBankURL:error:@
initWithData_soundBankURL_error :: (IsAVMIDIPlayer avmidiPlayer, IsNSData data_, IsNSURL bankURL, IsNSError outError) => avmidiPlayer -> data_ -> bankURL -> outError -> IO (Id AVMIDIPlayer)
initWithData_soundBankURL_error avmidiPlayer  data_ bankURL outError =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr bankURL $ \raw_bankURL ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avmidiPlayer (mkSelector "initWithData:soundBankURL:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_bankURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | prepareToPlay
--
-- Get ready to play the sequence by prerolling all events
--
-- Happens automatically on play if it has not already been called, but may produce a delay in startup.
--
-- ObjC selector: @- prepareToPlay@
prepareToPlay :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO ()
prepareToPlay avmidiPlayer  =
  sendMsg avmidiPlayer (mkSelector "prepareToPlay") retVoid []

-- | play:
--
-- Play the sequence.
--
-- ObjC selector: @- play:@
play :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> Ptr () -> IO ()
play avmidiPlayer  completionHandler =
  sendMsg avmidiPlayer (mkSelector "play:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | stop
--
-- Stop playing the sequence.
--
-- ObjC selector: @- stop@
stop :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO ()
stop avmidiPlayer  =
  sendMsg avmidiPlayer (mkSelector "stop") retVoid []

-- | duration
--
-- The length of the currently loaded file in seconds.
--
-- ObjC selector: @- duration@
duration :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO CDouble
duration avmidiPlayer  =
  sendMsg avmidiPlayer (mkSelector "duration") retCDouble []

-- | playing
--
-- Indicates whether or not the player is playing
--
-- ObjC selector: @- playing@
playing :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO Bool
playing avmidiPlayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avmidiPlayer (mkSelector "playing") retCULong []

-- | rate
--
-- The playback rate of the player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- rate@
rate :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO CFloat
rate avmidiPlayer  =
  sendMsg avmidiPlayer (mkSelector "rate") retCFloat []

-- | rate
--
-- The playback rate of the player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> CFloat -> IO ()
setRate avmidiPlayer  value =
  sendMsg avmidiPlayer (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- | currentPosition
--
-- The current playback position in seconds
--
-- Setting this positions the player to the specified time.  No range checking on the time value is done. 		This can be set while the player is playing, in which case playback will resume at the new time.
--
-- ObjC selector: @- currentPosition@
currentPosition :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO CDouble
currentPosition avmidiPlayer  =
  sendMsg avmidiPlayer (mkSelector "currentPosition") retCDouble []

-- | currentPosition
--
-- The current playback position in seconds
--
-- Setting this positions the player to the specified time.  No range checking on the time value is done. 		This can be set while the player is playing, in which case playback will resume at the new time.
--
-- ObjC selector: @- setCurrentPosition:@
setCurrentPosition :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> CDouble -> IO ()
setCurrentPosition avmidiPlayer  value =
  sendMsg avmidiPlayer (mkSelector "setCurrentPosition:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:soundBankURL:error:@
initWithContentsOfURL_soundBankURL_errorSelector :: Selector
initWithContentsOfURL_soundBankURL_errorSelector = mkSelector "initWithContentsOfURL:soundBankURL:error:"

-- | @Selector@ for @initWithData:soundBankURL:error:@
initWithData_soundBankURL_errorSelector :: Selector
initWithData_soundBankURL_errorSelector = mkSelector "initWithData:soundBankURL:error:"

-- | @Selector@ for @prepareToPlay@
prepareToPlaySelector :: Selector
prepareToPlaySelector = mkSelector "prepareToPlay"

-- | @Selector@ for @play:@
playSelector :: Selector
playSelector = mkSelector "play:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @playing@
playingSelector :: Selector
playingSelector = mkSelector "playing"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @currentPosition@
currentPositionSelector :: Selector
currentPositionSelector = mkSelector "currentPosition"

-- | @Selector@ for @setCurrentPosition:@
setCurrentPositionSelector :: Selector
setCurrentPositionSelector = mkSelector "setCurrentPosition:"

