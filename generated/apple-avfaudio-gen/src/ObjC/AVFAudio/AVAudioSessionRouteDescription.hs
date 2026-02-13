{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A description of the input and output ports which comprise a route.
--
-- Generated bindings for @AVAudioSessionRouteDescription@.
module ObjC.AVFAudio.AVAudioSessionRouteDescription
  ( AVAudioSessionRouteDescription
  , IsAVAudioSessionRouteDescription(..)
  , inputs
  , outputs
  , inputsSelector
  , outputsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Flattened list of all input port descriptions associated with all the streams as part of the route.
--
-- ObjC selector: @- inputs@
inputs :: IsAVAudioSessionRouteDescription avAudioSessionRouteDescription => avAudioSessionRouteDescription -> IO (Id NSArray)
inputs avAudioSessionRouteDescription =
  sendMessage avAudioSessionRouteDescription inputsSelector

-- | Flattened list of all output port descriptions associated with all the streams as part of the route.
--
-- ObjC selector: @- outputs@
outputs :: IsAVAudioSessionRouteDescription avAudioSessionRouteDescription => avAudioSessionRouteDescription -> IO (Id NSArray)
outputs avAudioSessionRouteDescription =
  sendMessage avAudioSessionRouteDescription outputsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSArray)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector '[] (Id NSArray)
outputsSelector = mkSelector "outputs"

