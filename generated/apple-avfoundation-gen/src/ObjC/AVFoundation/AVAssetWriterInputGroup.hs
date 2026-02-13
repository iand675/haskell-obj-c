{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetWriterInputGroup@.
module ObjC.AVFoundation.AVAssetWriterInputGroup
  ( AVAssetWriterInputGroup
  , IsAVAssetWriterInputGroup(..)
  , init_
  , new
  , assetWriterInputGroupWithInputs_defaultInput
  , initWithInputs_defaultInput
  , inputs
  , defaultInput
  , assetWriterInputGroupWithInputs_defaultInputSelector
  , defaultInputSelector
  , initSelector
  , initWithInputs_defaultInputSelector
  , inputsSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetWriterInputGroup avAssetWriterInputGroup => avAssetWriterInputGroup -> IO (Id AVAssetWriterInputGroup)
init_ avAssetWriterInputGroup =
  sendOwnedMessage avAssetWriterInputGroup initSelector

-- | @+ new@
new :: IO (Id AVAssetWriterInputGroup)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputGroup"
    sendOwnedClassMessage cls' newSelector

-- | @+ assetWriterInputGroupWithInputs:defaultInput:@
assetWriterInputGroupWithInputs_defaultInput :: (IsNSArray inputs, IsAVAssetWriterInput defaultInput) => inputs -> defaultInput -> IO (Id AVAssetWriterInputGroup)
assetWriterInputGroupWithInputs_defaultInput inputs defaultInput =
  do
    cls' <- getRequiredClass "AVAssetWriterInputGroup"
    sendClassMessage cls' assetWriterInputGroupWithInputs_defaultInputSelector (toNSArray inputs) (toAVAssetWriterInput defaultInput)

-- | @- initWithInputs:defaultInput:@
initWithInputs_defaultInput :: (IsAVAssetWriterInputGroup avAssetWriterInputGroup, IsNSArray inputs, IsAVAssetWriterInput defaultInput) => avAssetWriterInputGroup -> inputs -> defaultInput -> IO (Id AVAssetWriterInputGroup)
initWithInputs_defaultInput avAssetWriterInputGroup inputs defaultInput =
  sendOwnedMessage avAssetWriterInputGroup initWithInputs_defaultInputSelector (toNSArray inputs) (toAVAssetWriterInput defaultInput)

-- | inputs
--
-- The inputs grouped together by the receiver.
--
-- The value of this property is an NSArray containing concrete instances of AVAssetWriterInput.
--
-- ObjC selector: @- inputs@
inputs :: IsAVAssetWriterInputGroup avAssetWriterInputGroup => avAssetWriterInputGroup -> IO (Id NSArray)
inputs avAssetWriterInputGroup =
  sendMessage avAssetWriterInputGroup inputsSelector

-- | defaultInput
--
-- The input designated at the defaultInput of the receiver.
--
-- The value of this property is a concrete instance of AVAssetWriterInput.
--
-- ObjC selector: @- defaultInput@
defaultInput :: IsAVAssetWriterInputGroup avAssetWriterInputGroup => avAssetWriterInputGroup -> IO (Id AVAssetWriterInput)
defaultInput avAssetWriterInputGroup =
  sendMessage avAssetWriterInputGroup defaultInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetWriterInputGroup)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetWriterInputGroup)
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputGroupWithInputs:defaultInput:@
assetWriterInputGroupWithInputs_defaultInputSelector :: Selector '[Id NSArray, Id AVAssetWriterInput] (Id AVAssetWriterInputGroup)
assetWriterInputGroupWithInputs_defaultInputSelector = mkSelector "assetWriterInputGroupWithInputs:defaultInput:"

-- | @Selector@ for @initWithInputs:defaultInput:@
initWithInputs_defaultInputSelector :: Selector '[Id NSArray, Id AVAssetWriterInput] (Id AVAssetWriterInputGroup)
initWithInputs_defaultInputSelector = mkSelector "initWithInputs:defaultInput:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSArray)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @defaultInput@
defaultInputSelector :: Selector '[] (Id AVAssetWriterInput)
defaultInputSelector = mkSelector "defaultInput"

