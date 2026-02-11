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

-- | Flattened list of all input port descriptions associated with all the streams as part of the route.
--
-- ObjC selector: @- inputs@
inputs :: IsAVAudioSessionRouteDescription avAudioSessionRouteDescription => avAudioSessionRouteDescription -> IO (Id NSArray)
inputs avAudioSessionRouteDescription  =
    sendMsg avAudioSessionRouteDescription (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Flattened list of all output port descriptions associated with all the streams as part of the route.
--
-- ObjC selector: @- outputs@
outputs :: IsAVAudioSessionRouteDescription avAudioSessionRouteDescription => avAudioSessionRouteDescription -> IO (Id NSArray)
outputs avAudioSessionRouteDescription  =
    sendMsg avAudioSessionRouteDescription (mkSelector "outputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector
outputsSelector = mkSelector "outputs"

