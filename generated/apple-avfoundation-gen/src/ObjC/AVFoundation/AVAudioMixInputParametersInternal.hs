{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioMixInputParameters
--
-- Provides time-varying parameters to apply to an input of an audio mix. Audio volume is currently supported as a time-varying parameter.
--
-- Use an instance of AVAudioMixInputParameters to apply audio volume ramps for an input to an audio mix. AVAudioMixInputParameters are associated with audio tracks via the trackID property.
--
-- Notes on audio volume ramps:
--
-- Before the first time at which a volume is set, a volume of 1.0 used; after the last time for which a volume has been set, the last volume is used. Within the timeRange of a volume ramp, the volume is interpolated between the startVolume and endVolume of the ramp. For example, setting the volume to 1.0 at time 0 and also setting a volume ramp from a volume of 0.5 to 0.2 with a timeRange of [4.0, 5.0] results in an audio volume parameters that hold the volume constant at 1.0 from 0.0 sec to 4.0 sec, then cause it to jump to 0.5 and descend to 0.2 from 4.0 sec to 9.0 sec, holding constant at 0.2 thereafter.
--
-- Generated bindings for @AVAudioMixInputParametersInternal@.
module ObjC.AVFoundation.AVAudioMixInputParametersInternal
  ( AVAudioMixInputParametersInternal
  , IsAVAudioMixInputParametersInternal(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

