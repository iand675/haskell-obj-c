{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents an audio session
--
-- Generated bindings for @BEAudioSession@.
module ObjC.BrowserEngineCore.BEAudioSession
  ( BEAudioSession
  , IsBEAudioSession(..)
  , initWithAudioSession
  , setPreferredOutput_error
  , availableOutputs
  , preferredOutput
  , availableOutputsSelector
  , initWithAudioSessionSelector
  , preferredOutputSelector
  , setPreferredOutput_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineCore.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a BE audio session from an  AV audio session
--
-- - Parameters:   - session: The AV audio session
--
-- ObjC selector: @- initWithAudioSession:@
initWithAudioSession :: (IsBEAudioSession beAudioSession, IsAVAudioSession audioSession) => beAudioSession -> audioSession -> IO (Id BEAudioSession)
initWithAudioSession beAudioSession audioSession =
  sendOwnedMessage beAudioSession initWithAudioSessionSelector (toAVAudioSession audioSession)

-- | Select a preferred output port for audio routing.    Setting a nil value will clear the preference.
--
-- ObjC selector: @- setPreferredOutput:error:@
setPreferredOutput_error :: (IsBEAudioSession beAudioSession, IsAVAudioSessionPortDescription outPort, IsNSError outError) => beAudioSession -> outPort -> outError -> IO Bool
setPreferredOutput_error beAudioSession outPort outError =
  sendMessage beAudioSession setPreferredOutput_errorSelector (toAVAudioSessionPortDescription outPort) (toNSError outError)

-- | Gets the set of output ports that are available for routing.
--
-- ObjC selector: @- availableOutputs@
availableOutputs :: IsBEAudioSession beAudioSession => beAudioSession -> IO (Id NSArray)
availableOutputs beAudioSession =
  sendMessage beAudioSession availableOutputsSelector

-- | Get the preferred output port.  Will be nil if no preference has been set.
--
-- ObjC selector: @- preferredOutput@
preferredOutput :: IsBEAudioSession beAudioSession => beAudioSession -> IO (Id AVAudioSessionPortDescription)
preferredOutput beAudioSession =
  sendMessage beAudioSession preferredOutputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioSession:@
initWithAudioSessionSelector :: Selector '[Id AVAudioSession] (Id BEAudioSession)
initWithAudioSessionSelector = mkSelector "initWithAudioSession:"

-- | @Selector@ for @setPreferredOutput:error:@
setPreferredOutput_errorSelector :: Selector '[Id AVAudioSessionPortDescription, Id NSError] Bool
setPreferredOutput_errorSelector = mkSelector "setPreferredOutput:error:"

-- | @Selector@ for @availableOutputs@
availableOutputsSelector :: Selector '[] (Id NSArray)
availableOutputsSelector = mkSelector "availableOutputs"

-- | @Selector@ for @preferredOutput@
preferredOutputSelector :: Selector '[] (Id AVAudioSessionPortDescription)
preferredOutputSelector = mkSelector "preferredOutput"

