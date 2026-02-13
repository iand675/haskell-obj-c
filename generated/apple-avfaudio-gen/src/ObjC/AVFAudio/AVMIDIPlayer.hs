{-# LANGUAGE DataKinds #-}
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
  , currentPositionSelector
  , durationSelector
  , initWithContentsOfURL_soundBankURL_errorSelector
  , initWithData_soundBankURL_errorSelector
  , playSelector
  , playingSelector
  , prepareToPlaySelector
  , rateSelector
  , setCurrentPositionSelector
  , setRateSelector
  , stopSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithContentsOfURL_soundBankURL_error avmidiPlayer inURL bankURL outError =
  sendOwnedMessage avmidiPlayer initWithContentsOfURL_soundBankURL_errorSelector (toNSURL inURL) (toNSURL bankURL) (toNSError outError)

-- | initWithData:soundBankURL:error:
--
-- Create a player with the contents of the data object
--
-- 'bankURL' should contain the path to a SoundFont2 or DLS bank to be used		by the MIDI synthesizer.  For OSX it can be set to nil for the default,		but for iOS it must always refer to a valid bank file.
--
-- ObjC selector: @- initWithData:soundBankURL:error:@
initWithData_soundBankURL_error :: (IsAVMIDIPlayer avmidiPlayer, IsNSData data_, IsNSURL bankURL, IsNSError outError) => avmidiPlayer -> data_ -> bankURL -> outError -> IO (Id AVMIDIPlayer)
initWithData_soundBankURL_error avmidiPlayer data_ bankURL outError =
  sendOwnedMessage avmidiPlayer initWithData_soundBankURL_errorSelector (toNSData data_) (toNSURL bankURL) (toNSError outError)

-- | prepareToPlay
--
-- Get ready to play the sequence by prerolling all events
--
-- Happens automatically on play if it has not already been called, but may produce a delay in startup.
--
-- ObjC selector: @- prepareToPlay@
prepareToPlay :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO ()
prepareToPlay avmidiPlayer =
  sendMessage avmidiPlayer prepareToPlaySelector

-- | play:
--
-- Play the sequence.
--
-- ObjC selector: @- play:@
play :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> Ptr () -> IO ()
play avmidiPlayer completionHandler =
  sendMessage avmidiPlayer playSelector completionHandler

-- | stop
--
-- Stop playing the sequence.
--
-- ObjC selector: @- stop@
stop :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO ()
stop avmidiPlayer =
  sendMessage avmidiPlayer stopSelector

-- | duration
--
-- The length of the currently loaded file in seconds.
--
-- ObjC selector: @- duration@
duration :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO CDouble
duration avmidiPlayer =
  sendMessage avmidiPlayer durationSelector

-- | playing
--
-- Indicates whether or not the player is playing
--
-- ObjC selector: @- playing@
playing :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO Bool
playing avmidiPlayer =
  sendMessage avmidiPlayer playingSelector

-- | rate
--
-- The playback rate of the player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- rate@
rate :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO CFloat
rate avmidiPlayer =
  sendMessage avmidiPlayer rateSelector

-- | rate
--
-- The playback rate of the player
--
-- 1.0 is normal playback rate.  Rate must be > 0.0.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> CFloat -> IO ()
setRate avmidiPlayer value =
  sendMessage avmidiPlayer setRateSelector value

-- | currentPosition
--
-- The current playback position in seconds
--
-- Setting this positions the player to the specified time.  No range checking on the time value is done. 		This can be set while the player is playing, in which case playback will resume at the new time.
--
-- ObjC selector: @- currentPosition@
currentPosition :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> IO CDouble
currentPosition avmidiPlayer =
  sendMessage avmidiPlayer currentPositionSelector

-- | currentPosition
--
-- The current playback position in seconds
--
-- Setting this positions the player to the specified time.  No range checking on the time value is done. 		This can be set while the player is playing, in which case playback will resume at the new time.
--
-- ObjC selector: @- setCurrentPosition:@
setCurrentPosition :: IsAVMIDIPlayer avmidiPlayer => avmidiPlayer -> CDouble -> IO ()
setCurrentPosition avmidiPlayer value =
  sendMessage avmidiPlayer setCurrentPositionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:soundBankURL:error:@
initWithContentsOfURL_soundBankURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] (Id AVMIDIPlayer)
initWithContentsOfURL_soundBankURL_errorSelector = mkSelector "initWithContentsOfURL:soundBankURL:error:"

-- | @Selector@ for @initWithData:soundBankURL:error:@
initWithData_soundBankURL_errorSelector :: Selector '[Id NSData, Id NSURL, Id NSError] (Id AVMIDIPlayer)
initWithData_soundBankURL_errorSelector = mkSelector "initWithData:soundBankURL:error:"

-- | @Selector@ for @prepareToPlay@
prepareToPlaySelector :: Selector '[] ()
prepareToPlaySelector = mkSelector "prepareToPlay"

-- | @Selector@ for @play:@
playSelector :: Selector '[Ptr ()] ()
playSelector = mkSelector "play:"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @playing@
playingSelector :: Selector '[] Bool
playingSelector = mkSelector "playing"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @currentPosition@
currentPositionSelector :: Selector '[] CDouble
currentPositionSelector = mkSelector "currentPosition"

-- | @Selector@ for @setCurrentPosition:@
setCurrentPositionSelector :: Selector '[CDouble] ()
setCurrentPositionSelector = mkSelector "setCurrentPosition:"

