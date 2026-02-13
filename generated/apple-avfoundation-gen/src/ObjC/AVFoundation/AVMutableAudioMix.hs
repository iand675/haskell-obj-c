{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableAudioMix@.
module ObjC.AVFoundation.AVMutableAudioMix
  ( AVMutableAudioMix
  , IsAVMutableAudioMix(..)
  , audioMix
  , inputParameters
  , setInputParameters
  , audioMixSelector
  , inputParametersSelector
  , setInputParametersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ audioMix@
audioMix :: IO (Id AVMutableAudioMix)
audioMix  =
  do
    cls' <- getRequiredClass "AVMutableAudioMix"
    sendClassMessage cls' audioMixSelector

-- | inputParameters
--
-- Indicates parameters for inputs to the mix; an NSArray of instances of AVAudioMixInputParameters.
--
-- Note that an instance of AVAudioMixInputParameters is not required for each audio track that contributes to the mix; audio for those without associated AVAudioMixInputParameters will be included in the mix, processed according to default behavior.
--
-- ObjC selector: @- inputParameters@
inputParameters :: IsAVMutableAudioMix avMutableAudioMix => avMutableAudioMix -> IO (Id NSArray)
inputParameters avMutableAudioMix =
  sendMessage avMutableAudioMix inputParametersSelector

-- | inputParameters
--
-- Indicates parameters for inputs to the mix; an NSArray of instances of AVAudioMixInputParameters.
--
-- Note that an instance of AVAudioMixInputParameters is not required for each audio track that contributes to the mix; audio for those without associated AVAudioMixInputParameters will be included in the mix, processed according to default behavior.
--
-- ObjC selector: @- setInputParameters:@
setInputParameters :: (IsAVMutableAudioMix avMutableAudioMix, IsNSArray value) => avMutableAudioMix -> value -> IO ()
setInputParameters avMutableAudioMix value =
  sendMessage avMutableAudioMix setInputParametersSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioMix@
audioMixSelector :: Selector '[] (Id AVMutableAudioMix)
audioMixSelector = mkSelector "audioMix"

-- | @Selector@ for @inputParameters@
inputParametersSelector :: Selector '[] (Id NSArray)
inputParametersSelector = mkSelector "inputParameters"

-- | @Selector@ for @setInputParameters:@
setInputParametersSelector :: Selector '[Id NSArray] ()
setInputParametersSelector = mkSelector "setInputParameters:"

