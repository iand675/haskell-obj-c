{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- inputParameters@
inputParameters :: IsAVAudioMix avAudioMix => avAudioMix -> IO (Id NSArray)
inputParameters avAudioMix =
  sendMessage avAudioMix inputParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @inputParameters@
inputParametersSelector :: Selector '[] (Id NSArray)
inputParametersSelector = mkSelector "inputParameters"

