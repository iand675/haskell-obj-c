{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Wraps a v2 audio unit in an AUAudioUnit subclass.
--
-- Implementors of version 3 audio units may derive their implementations from		AUAudioUnitV2Bridge. It expects the component description with which it is initialized to		refer to a registered component with a v2 implementation using an		AudioComponentFactoryFunction. The bridge will instantiate the v2 audio unit via the factory		function and communicate it with it using the v2 AudioUnit API's (AudioUnitSetProperty,		etc.)
--
-- Hosts should not access this class; it will be instantiated when needed when creating an		AUAudioUnit.
--
-- Generated bindings for @AUAudioUnitV2Bridge@.
module ObjC.AudioToolbox.AUAudioUnitV2Bridge
  ( AUAudioUnitV2Bridge
  , IsAUAudioUnitV2Bridge(..)
  , audioUnit
  , audioUnitSelector


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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | audioUnit
--
-- The underlying v2 AudioUnit
--
-- We generally discourage interacting with the underlying v2 AudioUnit directly and		recommend using the v3 equivalent methods and properties from AUAudioUnitV2Bridge.
--
-- In some rare cases it may be desirable to interact with the v2 AudioUnit.		For example, a v2 plugin may define custom properties that are not bridged to v3.		Implementors can sublcass AUAudioUnitV2Bridge and call the v2 API methods		AudioUnitGetProperty / AudioUnitSetProperty with the v2 AudioUnit.
--
-- ObjC selector: @- audioUnit@
audioUnit :: IsAUAudioUnitV2Bridge auAudioUnitV2Bridge => auAudioUnitV2Bridge -> IO (Ptr ())
audioUnit auAudioUnitV2Bridge  =
  fmap castPtr $ sendMsg auAudioUnitV2Bridge (mkSelector "audioUnit") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector
audioUnitSelector = mkSelector "audioUnit"

