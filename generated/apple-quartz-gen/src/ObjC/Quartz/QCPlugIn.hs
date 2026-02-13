{-# LANGUAGE DataKinds #-}
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
  , addInputPortWithType_forKey_withAttributesSelector
  , addOutputPortWithType_forKey_withAttributesSelector
  , attributesForPropertyPortWithKeySelector
  , attributesSelector
  , createViewControllerSelector
  , didValueForInputKeyChangeSelector
  , disableExecutionSelector
  , enableExecutionSelector
  , execute_atTime_withArgumentsSelector
  , executionTimeForContext_atTime_withArgumentsSelector
  , loadPlugInAtPathSelector
  , plugInKeysSelector
  , registerPlugInClassSelector
  , removeInputPortForKeySelector
  , removeOutputPortForKeySelector
  , serializedValueForKeySelector
  , setSerializedValue_forKeySelector
  , setValue_forOutputKeySelector
  , sortedPropertyPortKeysSelector
  , startExecutionSelector
  , stopExecutionSelector
  , valueForInputKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ attributes@
attributes :: IO (Id NSDictionary)
attributes  =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMessage cls' attributesSelector

-- | @+ attributesForPropertyPortWithKey:@
attributesForPropertyPortWithKey :: IsNSString key => key -> IO (Id NSDictionary)
attributesForPropertyPortWithKey key =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMessage cls' attributesForPropertyPortWithKeySelector (toNSString key)

-- | @+ sortedPropertyPortKeys@
sortedPropertyPortKeys :: IO (Id NSArray)
sortedPropertyPortKeys  =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMessage cls' sortedPropertyPortKeysSelector

-- | @+ plugInKeys@
plugInKeys :: IO (Id NSArray)
plugInKeys  =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMessage cls' plugInKeysSelector

-- | @- startExecution:@
startExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO Bool
startExecution qcPlugIn context =
  sendMessage qcPlugIn startExecutionSelector context

-- | @- enableExecution:@
enableExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO ()
enableExecution qcPlugIn context =
  sendMessage qcPlugIn enableExecutionSelector context

-- | @- executionTimeForContext:atTime:withArguments:@
executionTimeForContext_atTime_withArguments :: (IsQCPlugIn qcPlugIn, IsNSDictionary arguments) => qcPlugIn -> RawId -> CDouble -> arguments -> IO CDouble
executionTimeForContext_atTime_withArguments qcPlugIn context time arguments =
  sendMessage qcPlugIn executionTimeForContext_atTime_withArgumentsSelector context time (toNSDictionary arguments)

-- | @- execute:atTime:withArguments:@
execute_atTime_withArguments :: (IsQCPlugIn qcPlugIn, IsNSDictionary arguments) => qcPlugIn -> RawId -> CDouble -> arguments -> IO Bool
execute_atTime_withArguments qcPlugIn context time arguments =
  sendMessage qcPlugIn execute_atTime_withArgumentsSelector context time (toNSDictionary arguments)

-- | @- disableExecution:@
disableExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO ()
disableExecution qcPlugIn context =
  sendMessage qcPlugIn disableExecutionSelector context

-- | @- stopExecution:@
stopExecution :: IsQCPlugIn qcPlugIn => qcPlugIn -> RawId -> IO ()
stopExecution qcPlugIn context =
  sendMessage qcPlugIn stopExecutionSelector context

-- | @- serializedValueForKey:@
serializedValueForKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO RawId
serializedValueForKey qcPlugIn key =
  sendMessage qcPlugIn serializedValueForKeySelector (toNSString key)

-- | @- setSerializedValue:forKey:@
setSerializedValue_forKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> RawId -> key -> IO ()
setSerializedValue_forKey qcPlugIn serializedValue key =
  sendMessage qcPlugIn setSerializedValue_forKeySelector serializedValue (toNSString key)

-- | @- createViewController@
createViewController :: IsQCPlugIn qcPlugIn => qcPlugIn -> IO (Id QCPlugInViewController)
createViewController qcPlugIn =
  sendMessage qcPlugIn createViewControllerSelector

-- | @+ loadPlugInAtPath:@
loadPlugInAtPath :: IsNSString path => path -> IO Bool
loadPlugInAtPath path =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMessage cls' loadPlugInAtPathSelector (toNSString path)

-- | @+ registerPlugInClass:@
registerPlugInClass :: Class -> IO ()
registerPlugInClass aClass =
  do
    cls' <- getRequiredClass "QCPlugIn"
    sendClassMessage cls' registerPlugInClassSelector aClass

-- | @- didValueForInputKeyChange:@
didValueForInputKeyChange :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO Bool
didValueForInputKeyChange qcPlugIn key =
  sendMessage qcPlugIn didValueForInputKeyChangeSelector (toNSString key)

-- | @- valueForInputKey:@
valueForInputKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO RawId
valueForInputKey qcPlugIn key =
  sendMessage qcPlugIn valueForInputKeySelector (toNSString key)

-- | @- setValue:forOutputKey:@
setValue_forOutputKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> RawId -> key -> IO Bool
setValue_forOutputKey qcPlugIn value key =
  sendMessage qcPlugIn setValue_forOutputKeySelector value (toNSString key)

-- | @- addInputPortWithType:forKey:withAttributes:@
addInputPortWithType_forKey_withAttributes :: (IsQCPlugIn qcPlugIn, IsNSString type_, IsNSString key, IsNSDictionary attributes) => qcPlugIn -> type_ -> key -> attributes -> IO ()
addInputPortWithType_forKey_withAttributes qcPlugIn type_ key attributes =
  sendMessage qcPlugIn addInputPortWithType_forKey_withAttributesSelector (toNSString type_) (toNSString key) (toNSDictionary attributes)

-- | @- removeInputPortForKey:@
removeInputPortForKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO ()
removeInputPortForKey qcPlugIn key =
  sendMessage qcPlugIn removeInputPortForKeySelector (toNSString key)

-- | @- addOutputPortWithType:forKey:withAttributes:@
addOutputPortWithType_forKey_withAttributes :: (IsQCPlugIn qcPlugIn, IsNSString type_, IsNSString key, IsNSDictionary attributes) => qcPlugIn -> type_ -> key -> attributes -> IO ()
addOutputPortWithType_forKey_withAttributes qcPlugIn type_ key attributes =
  sendMessage qcPlugIn addOutputPortWithType_forKey_withAttributesSelector (toNSString type_) (toNSString key) (toNSDictionary attributes)

-- | @- removeOutputPortForKey:@
removeOutputPortForKey :: (IsQCPlugIn qcPlugIn, IsNSString key) => qcPlugIn -> key -> IO ()
removeOutputPortForKey qcPlugIn key =
  sendMessage qcPlugIn removeOutputPortForKeySelector (toNSString key)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @attributesForPropertyPortWithKey:@
attributesForPropertyPortWithKeySelector :: Selector '[Id NSString] (Id NSDictionary)
attributesForPropertyPortWithKeySelector = mkSelector "attributesForPropertyPortWithKey:"

-- | @Selector@ for @sortedPropertyPortKeys@
sortedPropertyPortKeysSelector :: Selector '[] (Id NSArray)
sortedPropertyPortKeysSelector = mkSelector "sortedPropertyPortKeys"

-- | @Selector@ for @plugInKeys@
plugInKeysSelector :: Selector '[] (Id NSArray)
plugInKeysSelector = mkSelector "plugInKeys"

-- | @Selector@ for @startExecution:@
startExecutionSelector :: Selector '[RawId] Bool
startExecutionSelector = mkSelector "startExecution:"

-- | @Selector@ for @enableExecution:@
enableExecutionSelector :: Selector '[RawId] ()
enableExecutionSelector = mkSelector "enableExecution:"

-- | @Selector@ for @executionTimeForContext:atTime:withArguments:@
executionTimeForContext_atTime_withArgumentsSelector :: Selector '[RawId, CDouble, Id NSDictionary] CDouble
executionTimeForContext_atTime_withArgumentsSelector = mkSelector "executionTimeForContext:atTime:withArguments:"

-- | @Selector@ for @execute:atTime:withArguments:@
execute_atTime_withArgumentsSelector :: Selector '[RawId, CDouble, Id NSDictionary] Bool
execute_atTime_withArgumentsSelector = mkSelector "execute:atTime:withArguments:"

-- | @Selector@ for @disableExecution:@
disableExecutionSelector :: Selector '[RawId] ()
disableExecutionSelector = mkSelector "disableExecution:"

-- | @Selector@ for @stopExecution:@
stopExecutionSelector :: Selector '[RawId] ()
stopExecutionSelector = mkSelector "stopExecution:"

-- | @Selector@ for @serializedValueForKey:@
serializedValueForKeySelector :: Selector '[Id NSString] RawId
serializedValueForKeySelector = mkSelector "serializedValueForKey:"

-- | @Selector@ for @setSerializedValue:forKey:@
setSerializedValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setSerializedValue_forKeySelector = mkSelector "setSerializedValue:forKey:"

-- | @Selector@ for @createViewController@
createViewControllerSelector :: Selector '[] (Id QCPlugInViewController)
createViewControllerSelector = mkSelector "createViewController"

-- | @Selector@ for @loadPlugInAtPath:@
loadPlugInAtPathSelector :: Selector '[Id NSString] Bool
loadPlugInAtPathSelector = mkSelector "loadPlugInAtPath:"

-- | @Selector@ for @registerPlugInClass:@
registerPlugInClassSelector :: Selector '[Class] ()
registerPlugInClassSelector = mkSelector "registerPlugInClass:"

-- | @Selector@ for @didValueForInputKeyChange:@
didValueForInputKeyChangeSelector :: Selector '[Id NSString] Bool
didValueForInputKeyChangeSelector = mkSelector "didValueForInputKeyChange:"

-- | @Selector@ for @valueForInputKey:@
valueForInputKeySelector :: Selector '[Id NSString] RawId
valueForInputKeySelector = mkSelector "valueForInputKey:"

-- | @Selector@ for @setValue:forOutputKey:@
setValue_forOutputKeySelector :: Selector '[RawId, Id NSString] Bool
setValue_forOutputKeySelector = mkSelector "setValue:forOutputKey:"

-- | @Selector@ for @addInputPortWithType:forKey:withAttributes:@
addInputPortWithType_forKey_withAttributesSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] ()
addInputPortWithType_forKey_withAttributesSelector = mkSelector "addInputPortWithType:forKey:withAttributes:"

-- | @Selector@ for @removeInputPortForKey:@
removeInputPortForKeySelector :: Selector '[Id NSString] ()
removeInputPortForKeySelector = mkSelector "removeInputPortForKey:"

-- | @Selector@ for @addOutputPortWithType:forKey:withAttributes:@
addOutputPortWithType_forKey_withAttributesSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] ()
addOutputPortWithType_forKey_withAttributesSelector = mkSelector "addOutputPortWithType:forKey:withAttributes:"

-- | @Selector@ for @removeOutputPortForKey:@
removeOutputPortForKeySelector :: Selector '[Id NSString] ()
removeOutputPortForKeySelector = mkSelector "removeOutputPortForKey:"

