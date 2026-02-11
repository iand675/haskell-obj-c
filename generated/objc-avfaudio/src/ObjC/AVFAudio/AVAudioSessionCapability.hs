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
  , supportedSelector
  , enabledSelector


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

-- | A Boolean value that indicates whether the capability is supported.
--
-- ObjC selector: @- supported@
supported :: IsAVAudioSessionCapability avAudioSessionCapability => avAudioSessionCapability -> IO Bool
supported avAudioSessionCapability  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSessionCapability (mkSelector "supported") retCULong []

-- | A Boolean value that indicates whether the capability is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsAVAudioSessionCapability avAudioSessionCapability => avAudioSessionCapability -> IO Bool
enabled avAudioSessionCapability  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSessionCapability (mkSelector "enabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

