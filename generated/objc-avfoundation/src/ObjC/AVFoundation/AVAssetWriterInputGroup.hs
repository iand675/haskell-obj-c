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
  , initSelector
  , newSelector
  , assetWriterInputGroupWithInputs_defaultInputSelector
  , initWithInputs_defaultInputSelector
  , inputsSelector
  , defaultInputSelector


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

-- | @- init@
init_ :: IsAVAssetWriterInputGroup avAssetWriterInputGroup => avAssetWriterInputGroup -> IO (Id AVAssetWriterInputGroup)
init_ avAssetWriterInputGroup  =
  sendMsg avAssetWriterInputGroup (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetWriterInputGroup)
new  =
  do
    cls' <- getRequiredClass "AVAssetWriterInputGroup"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ assetWriterInputGroupWithInputs:defaultInput:@
assetWriterInputGroupWithInputs_defaultInput :: (IsNSArray inputs, IsAVAssetWriterInput defaultInput) => inputs -> defaultInput -> IO (Id AVAssetWriterInputGroup)
assetWriterInputGroupWithInputs_defaultInput inputs defaultInput =
  do
    cls' <- getRequiredClass "AVAssetWriterInputGroup"
    withObjCPtr inputs $ \raw_inputs ->
      withObjCPtr defaultInput $ \raw_defaultInput ->
        sendClassMsg cls' (mkSelector "assetWriterInputGroupWithInputs:defaultInput:") (retPtr retVoid) [argPtr (castPtr raw_inputs :: Ptr ()), argPtr (castPtr raw_defaultInput :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithInputs:defaultInput:@
initWithInputs_defaultInput :: (IsAVAssetWriterInputGroup avAssetWriterInputGroup, IsNSArray inputs, IsAVAssetWriterInput defaultInput) => avAssetWriterInputGroup -> inputs -> defaultInput -> IO (Id AVAssetWriterInputGroup)
initWithInputs_defaultInput avAssetWriterInputGroup  inputs defaultInput =
withObjCPtr inputs $ \raw_inputs ->
  withObjCPtr defaultInput $ \raw_defaultInput ->
      sendMsg avAssetWriterInputGroup (mkSelector "initWithInputs:defaultInput:") (retPtr retVoid) [argPtr (castPtr raw_inputs :: Ptr ()), argPtr (castPtr raw_defaultInput :: Ptr ())] >>= ownedObject . castPtr

-- | inputs
--
-- The inputs grouped together by the receiver.
--
-- The value of this property is an NSArray containing concrete instances of AVAssetWriterInput.
--
-- ObjC selector: @- inputs@
inputs :: IsAVAssetWriterInputGroup avAssetWriterInputGroup => avAssetWriterInputGroup -> IO (Id NSArray)
inputs avAssetWriterInputGroup  =
  sendMsg avAssetWriterInputGroup (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultInput
--
-- The input designated at the defaultInput of the receiver.
--
-- The value of this property is a concrete instance of AVAssetWriterInput.
--
-- ObjC selector: @- defaultInput@
defaultInput :: IsAVAssetWriterInputGroup avAssetWriterInputGroup => avAssetWriterInputGroup -> IO (Id AVAssetWriterInput)
defaultInput avAssetWriterInputGroup  =
  sendMsg avAssetWriterInputGroup (mkSelector "defaultInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetWriterInputGroupWithInputs:defaultInput:@
assetWriterInputGroupWithInputs_defaultInputSelector :: Selector
assetWriterInputGroupWithInputs_defaultInputSelector = mkSelector "assetWriterInputGroupWithInputs:defaultInput:"

-- | @Selector@ for @initWithInputs:defaultInput:@
initWithInputs_defaultInputSelector :: Selector
initWithInputs_defaultInputSelector = mkSelector "initWithInputs:defaultInput:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @defaultInput@
defaultInputSelector :: Selector
defaultInputSelector = mkSelector "defaultInput"

