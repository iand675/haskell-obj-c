{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCPlugIn@.
module ObjC.Quartz.QCPlugIn
  ( QCPlugIn
  , IsQCPlugIn(..)
  , attributes
  , attributesForPropertyPortWithKey
  , sortedPropertyPortKeys
  , plugInKeys
  , startExecution
  , enableExecution
  , executionTimeForContext_atTime_withArguments
  , execute_atTime_withArguments
  , disableExecution
  , stopExecution
  , serializedValueForKey
  , setSerializedValue_forKey
  , createViewController
  , loadPlugInAtPath
  , registerPlugInClass
  , didValueForInputKeyChange
  , valueForInputKey
  , setValue_forOutputKey
  , addInputPortWithType_forKey_withAttributes
  , removeInputPortForKey
  , addOutputPortWithType_forKey_withAttributes
  , removeOutputPortForKey
  , attributesSelector
  , attributesForPropertyPortWithKeySelector
  , sortedPropertyPortKeysSelector
  , plugInKeysSelector
  , startExecutionSelector
  , enableExecutionSelector
  , executionTimeForContext_atTime_withArgumentsSelector
  , execute_atTime_withArgumentsSelector
  , disableExecutionSelector
  , stopExecutionSelector
  , serializedValueForKeySelector
  , setSerializedValue_forKeySelector
  , createViewControllerSelector
  , loadPlugInAtPathSelector
  , registerPlugInClassSelector
  , didValueForInputKeyChangeSelector
  , valueForInputKeySelector
  , setValue_forOutputKeySelector
  , addInputPortWithType_forKey_withAttributesSelector
  , removeInputPortForKeySelector
  , addOutputPortWithType_forKey_withAttributesSelector
  , removeOutputPortForKeySelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ attributes@
attributes :: IO (Id NSDictionary)
attributes  =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMsg cls' (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ attributesForPropertyPortWithKey:@
attributesForPropertyPortWithKey :: IsNSString key => key -> IO (Id NSDictionary)
attributesForPropertyPortWithKey key =
  do
    cls' <- getRequiredClass "QCPlugIn"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "attributesForPropertyPortWithKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sortedPropertyPortKeys@
sortedPropertyPortKeys :: IO (Id NSArray)
sortedPropertyPortKeys  =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMsg cls' (mkSelector "sortedPropertyPortKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ plugInKeys@
plugInKeys :: IO (Id NSArray)
plugInKeys  =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMsg cls' (mkSelector "plugInKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startExecution:@
startExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO Bool
startExecution qcPlugIn  context =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcPlugIn (mkSelector "startExecution:") retCULong [argPtr (castPtr (unRawId context) :: Ptr ())]

-- | @- enableExecution:@
enableExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO ()
enableExecution qcPlugIn  context =
  sendMsg qcPlugIn (mkSelector "enableExecution:") retVoid [argPtr (castPtr (unRawId context) :: Ptr ())]

-- | @- executionTimeForContext:atTime:withArguments:@
executionTimeForContext_atTime_withArguments :: (IsQCPlugIn qcPlugIn, IsNSDictionary arguments) => qcPlugIn -> RawId -> CDouble -> arguments -> IO CDouble
executionTimeForContext_atTime_withArguments qcPlugIn  context time arguments =
withObjCPtr arguments $ \raw_arguments ->
    sendMsg qcPlugIn (mkSelector "executionTimeForContext:atTime:withArguments:") retCDouble [argPtr (castPtr (unRawId context) :: Ptr ()), argCDouble (fromIntegral time), argPtr (castPtr raw_arguments :: Ptr ())]

-- | @- execute:atTime:withArguments:@
execute_atTime_withArguments :: (IsQCPlugIn qcPlugIn, IsNSDictionary arguments) => qcPlugIn -> RawId -> CDouble -> arguments -> IO Bool
execute_atTime_withArguments qcPlugIn  context time arguments =
withObjCPtr arguments $ \raw_arguments ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcPlugIn (mkSelector "execute:atTime:withArguments:") retCULong [argPtr (castPtr (unRawId context) :: Ptr ()), argCDouble (fromIntegral time), argPtr (castPtr raw_arguments :: Ptr ())]

-- | @- disableExecution:@
disableExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO ()
disableExecution qcPlugIn  context =
  sendMsg qcPlugIn (mkSelector "disableExecution:") retVoid [argPtr (castPtr (unRawId context) :: Ptr ())]

-- | @- stopExecution:@
stopExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO ()
stopExecution qcPlugIn  context =
  sendMsg qcPlugIn (mkSelector "stopExecution:") retVoid [argPtr (castPtr (unRawId context) :: Ptr ())]

-- | @- serializedValueForKey:@
serializedValueForKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO RawId
serializedValueForKey qcPlugIn  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg qcPlugIn (mkSelector "serializedValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setSerializedValue:forKey:@
setSerializedValue_forKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> RawId -> key -> IO ()
setSerializedValue_forKey qcPlugIn  serializedValue key =
withObjCPtr key $ \raw_key ->
    sendMsg qcPlugIn (mkSelector "setSerializedValue:forKey:") retVoid [argPtr (castPtr (unRawId serializedValue) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- createViewController@
createViewController :: IsQCPlugIn qcPlugIn => qcPlugIn -> IO (Id QCPlugInViewController)
createViewController qcPlugIn  =
  sendMsg qcPlugIn (mkSelector "createViewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ loadPlugInAtPath:@
loadPlugInAtPath :: IsNSString path => path -> IO Bool
loadPlugInAtPath path =
  do
    cls' <- getRequiredClass "QCPlugIn"
    withObjCPtr path $ \raw_path ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "loadPlugInAtPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @+ registerPlugInClass:@
registerPlugInClass :: Class -> IO ()
registerPlugInClass aClass =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMsg cls' (mkSelector "registerPlugInClass:") retVoid [argPtr (unClass aClass)]

-- | @- didValueForInputKeyChange:@
didValueForInputKeyChange :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO Bool
didValueForInputKeyChange qcPlugIn  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcPlugIn (mkSelector "didValueForInputKeyChange:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- valueForInputKey:@
valueForInputKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO RawId
valueForInputKey qcPlugIn  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg qcPlugIn (mkSelector "valueForInputKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forOutputKey:@
setValue_forOutputKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> RawId -> key -> IO Bool
setValue_forOutputKey qcPlugIn  value key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcPlugIn (mkSelector "setValue:forOutputKey:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- addInputPortWithType:forKey:withAttributes:@
addInputPortWithType_forKey_withAttributes :: (IsQCPlugIn qcPlugIn, IsNSString type_, IsNSString key, IsNSDictionary attributes) => qcPlugIn -> type_ -> key -> attributes -> IO ()
addInputPortWithType_forKey_withAttributes qcPlugIn  type_ key attributes =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr key $ \raw_key ->
    withObjCPtr attributes $ \raw_attributes ->
        sendMsg qcPlugIn (mkSelector "addInputPortWithType:forKey:withAttributes:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- removeInputPortForKey:@
removeInputPortForKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO ()
removeInputPortForKey qcPlugIn  key =
withObjCPtr key $ \raw_key ->
    sendMsg qcPlugIn (mkSelector "removeInputPortForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- addOutputPortWithType:forKey:withAttributes:@
addOutputPortWithType_forKey_withAttributes :: (IsQCPlugIn qcPlugIn, IsNSString type_, IsNSString key, IsNSDictionary attributes) => qcPlugIn -> type_ -> key -> attributes -> IO ()
addOutputPortWithType_forKey_withAttributes qcPlugIn  type_ key attributes =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr key $ \raw_key ->
    withObjCPtr attributes $ \raw_attributes ->
        sendMsg qcPlugIn (mkSelector "addOutputPortWithType:forKey:withAttributes:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- removeOutputPortForKey:@
removeOutputPortForKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO ()
removeOutputPortForKey qcPlugIn  key =
withObjCPtr key $ \raw_key ->
    sendMsg qcPlugIn (mkSelector "removeOutputPortForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @attributesForPropertyPortWithKey:@
attributesForPropertyPortWithKeySelector :: Selector
attributesForPropertyPortWithKeySelector = mkSelector "attributesForPropertyPortWithKey:"

-- | @Selector@ for @sortedPropertyPortKeys@
sortedPropertyPortKeysSelector :: Selector
sortedPropertyPortKeysSelector = mkSelector "sortedPropertyPortKeys"

-- | @Selector@ for @plugInKeys@
plugInKeysSelector :: Selector
plugInKeysSelector = mkSelector "plugInKeys"

-- | @Selector@ for @startExecution:@
startExecutionSelector :: Selector
startExecutionSelector = mkSelector "startExecution:"

-- | @Selector@ for @enableExecution:@
enableExecutionSelector :: Selector
enableExecutionSelector = mkSelector "enableExecution:"

-- | @Selector@ for @executionTimeForContext:atTime:withArguments:@
executionTimeForContext_atTime_withArgumentsSelector :: Selector
executionTimeForContext_atTime_withArgumentsSelector = mkSelector "executionTimeForContext:atTime:withArguments:"

-- | @Selector@ for @execute:atTime:withArguments:@
execute_atTime_withArgumentsSelector :: Selector
execute_atTime_withArgumentsSelector = mkSelector "execute:atTime:withArguments:"

-- | @Selector@ for @disableExecution:@
disableExecutionSelector :: Selector
disableExecutionSelector = mkSelector "disableExecution:"

-- | @Selector@ for @stopExecution:@
stopExecutionSelector :: Selector
stopExecutionSelector = mkSelector "stopExecution:"

-- | @Selector@ for @serializedValueForKey:@
serializedValueForKeySelector :: Selector
serializedValueForKeySelector = mkSelector "serializedValueForKey:"

-- | @Selector@ for @setSerializedValue:forKey:@
setSerializedValue_forKeySelector :: Selector
setSerializedValue_forKeySelector = mkSelector "setSerializedValue:forKey:"

-- | @Selector@ for @createViewController@
createViewControllerSelector :: Selector
createViewControllerSelector = mkSelector "createViewController"

-- | @Selector@ for @loadPlugInAtPath:@
loadPlugInAtPathSelector :: Selector
loadPlugInAtPathSelector = mkSelector "loadPlugInAtPath:"

-- | @Selector@ for @registerPlugInClass:@
registerPlugInClassSelector :: Selector
registerPlugInClassSelector = mkSelector "registerPlugInClass:"

-- | @Selector@ for @didValueForInputKeyChange:@
didValueForInputKeyChangeSelector :: Selector
didValueForInputKeyChangeSelector = mkSelector "didValueForInputKeyChange:"

-- | @Selector@ for @valueForInputKey:@
valueForInputKeySelector :: Selector
valueForInputKeySelector = mkSelector "valueForInputKey:"

-- | @Selector@ for @setValue:forOutputKey:@
setValue_forOutputKeySelector :: Selector
setValue_forOutputKeySelector = mkSelector "setValue:forOutputKey:"

-- | @Selector@ for @addInputPortWithType:forKey:withAttributes:@
addInputPortWithType_forKey_withAttributesSelector :: Selector
addInputPortWithType_forKey_withAttributesSelector = mkSelector "addInputPortWithType:forKey:withAttributes:"

-- | @Selector@ for @removeInputPortForKey:@
removeInputPortForKeySelector :: Selector
removeInputPortForKeySelector = mkSelector "removeInputPortForKey:"

-- | @Selector@ for @addOutputPortWithType:forKey:withAttributes:@
addOutputPortWithType_forKey_withAttributesSelector :: Selector
addOutputPortWithType_forKey_withAttributesSelector = mkSelector "addOutputPortWithType:forKey:withAttributes:"

-- | @Selector@ for @removeOutputPortForKey:@
removeOutputPortForKeySelector :: Selector
removeOutputPortForKeySelector = mkSelector "removeOutputPortForKey:"

