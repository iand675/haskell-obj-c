{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAudioMix@.
module ObjC.AVFoundation.AVAudioMix
  ( AVAudioMix
  , IsAVAudioMix(..)
  , inputParameters
  , inputParametersSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- inputParameters@
inputParameters :: IsAVAudioMix avAudioMix => avAudioMix -> IO (Id NSArray)
inputParameters avAudioMix  =
  sendMsg avAudioMix (mkSelector "inputParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputParameters@
inputParametersSelector :: Selector
inputParametersSelector = mkSelector "inputParameters"

