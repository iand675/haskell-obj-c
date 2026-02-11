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

-- | @+ audioMix@
audioMix :: IO (Id AVMutableAudioMix)
audioMix  =
  do
    cls' <- getRequiredClass "AVMutableAudioMix"
    sendClassMsg cls' (mkSelector "audioMix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | inputParameters
--
-- Indicates parameters for inputs to the mix; an NSArray of instances of AVAudioMixInputParameters.
--
-- Note that an instance of AVAudioMixInputParameters is not required for each audio track that contributes to the mix; audio for those without associated AVAudioMixInputParameters will be included in the mix, processed according to default behavior.
--
-- ObjC selector: @- inputParameters@
inputParameters :: IsAVMutableAudioMix avMutableAudioMix => avMutableAudioMix -> IO (Id NSArray)
inputParameters avMutableAudioMix  =
  sendMsg avMutableAudioMix (mkSelector "inputParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | inputParameters
--
-- Indicates parameters for inputs to the mix; an NSArray of instances of AVAudioMixInputParameters.
--
-- Note that an instance of AVAudioMixInputParameters is not required for each audio track that contributes to the mix; audio for those without associated AVAudioMixInputParameters will be included in the mix, processed according to default behavior.
--
-- ObjC selector: @- setInputParameters:@
setInputParameters :: (IsAVMutableAudioMix avMutableAudioMix, IsNSArray value) => avMutableAudioMix -> value -> IO ()
setInputParameters avMutableAudioMix  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableAudioMix (mkSelector "setInputParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioMix@
audioMixSelector :: Selector
audioMixSelector = mkSelector "audioMix"

-- | @Selector@ for @inputParameters@
inputParametersSelector :: Selector
inputParametersSelector = mkSelector "inputParameters"

-- | @Selector@ for @setInputParameters:@
setInputParametersSelector :: Selector
setInputParametersSelector = mkSelector "setInputParameters:"

