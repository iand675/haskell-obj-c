{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes whether a specific capability is supported and if that capability is currently enabled
--
-- Generated bindings for @AVAudioSessionCapability@.
module ObjC.AVFAudio.AVAudioSessionCapability
  ( AVAudioSessionCapability
  , IsAVAudioSessionCapability(..)
  , supported
  , enabled
  , enabledSelector
  , supportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value that indicates whether the capability is supported.
--
-- ObjC selector: @- supported@
supported :: IsAVAudioSessionCapability avAudioSessionCapability => avAudioSessionCapability -> IO Bool
supported avAudioSessionCapability =
  sendMessage avAudioSessionCapability supportedSelector

-- | A Boolean value that indicates whether the capability is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsAVAudioSessionCapability avAudioSessionCapability => avAudioSessionCapability -> IO Bool
enabled avAudioSessionCapability =
  sendMessage avAudioSessionCapability enabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

