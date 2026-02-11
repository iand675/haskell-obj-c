{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ODRecordMap@.
module ObjC.OpenDirectory.ODRecordMap
  ( ODRecordMap
  , IsODRecordMap(..)
  , recordMap
  , attributeMapForStandardAttribute
  , setAttributeMap_forStandardAttribute
  , native
  , setNative
  , odPredicate
  , setOdPredicate
  , attributes
  , standardAttributeTypes
  , recordMapSelector
  , attributeMapForStandardAttributeSelector
  , setAttributeMap_forStandardAttributeSelector
  , nativeSelector
  , setNativeSelector
  , odPredicateSelector
  , setOdPredicateSelector
  , attributesSelector
  , standardAttributeTypesSelector


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

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | recordMap
--
-- Returns an initialized and autoreleased ODRecordMap object.
--
-- Returns an initialized and autoreleased ODRecordMap object.
--
-- ObjC selector: @+ recordMap@
recordMap :: IO (Id ODRecordMap)
recordMap  =
  do
    cls' <- getRequiredClass "ODRecordMap"
    sendClassMsg cls' (mkSelector "recordMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | attributeMapForStandardAttribute:
--
-- Returns an ODAttributeMap object for the given OD standard attribute.
--
-- Returns an ODAttributeMap object for the given OD standard attribute.
--
-- ObjC selector: @- attributeMapForStandardAttribute:@
attributeMapForStandardAttribute :: (IsODRecordMap odRecordMap, IsNSString standardAttribute) => odRecordMap -> standardAttribute -> IO (Id ODAttributeMap)
attributeMapForStandardAttribute odRecordMap  standardAttribute =
withObjCPtr standardAttribute $ \raw_standardAttribute ->
    sendMsg odRecordMap (mkSelector "attributeMapForStandardAttribute:") (retPtr retVoid) [argPtr (castPtr raw_standardAttribute :: Ptr ())] >>= retainedObject . castPtr

-- | setAttributeMap:forStandardAttribute:
--
-- Sets an ODAttributeMap object for a given OD standard attribute.
--
-- Sets an ODAttributeMap object for a given OD standard attribute.
--
-- ObjC selector: @- setAttributeMap:forStandardAttribute:@
setAttributeMap_forStandardAttribute :: (IsODRecordMap odRecordMap, IsODAttributeMap attributeMap, IsNSString standardAttribute) => odRecordMap -> attributeMap -> standardAttribute -> IO ()
setAttributeMap_forStandardAttribute odRecordMap  attributeMap standardAttribute =
withObjCPtr attributeMap $ \raw_attributeMap ->
  withObjCPtr standardAttribute $ \raw_standardAttribute ->
      sendMsg odRecordMap (mkSelector "setAttributeMap:forStandardAttribute:") retVoid [argPtr (castPtr raw_attributeMap :: Ptr ()), argPtr (castPtr raw_standardAttribute :: Ptr ())]

-- | @- native@
native :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSString)
native odRecordMap  =
  sendMsg odRecordMap (mkSelector "native") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNative:@
setNative :: (IsODRecordMap odRecordMap, IsNSString value) => odRecordMap -> value -> IO ()
setNative odRecordMap  value =
withObjCPtr value $ \raw_value ->
    sendMsg odRecordMap (mkSelector "setNative:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- odPredicate@
odPredicate :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSDictionary)
odPredicate odRecordMap  =
  sendMsg odRecordMap (mkSelector "odPredicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOdPredicate:@
setOdPredicate :: (IsODRecordMap odRecordMap, IsNSDictionary value) => odRecordMap -> value -> IO ()
setOdPredicate odRecordMap  value =
withObjCPtr value $ \raw_value ->
    sendMsg odRecordMap (mkSelector "setOdPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributes@
attributes :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSDictionary)
attributes odRecordMap  =
  sendMsg odRecordMap (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- standardAttributeTypes@
standardAttributeTypes :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSArray)
standardAttributeTypes odRecordMap  =
  sendMsg odRecordMap (mkSelector "standardAttributeTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recordMap@
recordMapSelector :: Selector
recordMapSelector = mkSelector "recordMap"

-- | @Selector@ for @attributeMapForStandardAttribute:@
attributeMapForStandardAttributeSelector :: Selector
attributeMapForStandardAttributeSelector = mkSelector "attributeMapForStandardAttribute:"

-- | @Selector@ for @setAttributeMap:forStandardAttribute:@
setAttributeMap_forStandardAttributeSelector :: Selector
setAttributeMap_forStandardAttributeSelector = mkSelector "setAttributeMap:forStandardAttribute:"

-- | @Selector@ for @native@
nativeSelector :: Selector
nativeSelector = mkSelector "native"

-- | @Selector@ for @setNative:@
setNativeSelector :: Selector
setNativeSelector = mkSelector "setNative:"

-- | @Selector@ for @odPredicate@
odPredicateSelector :: Selector
odPredicateSelector = mkSelector "odPredicate"

-- | @Selector@ for @setOdPredicate:@
setOdPredicateSelector :: Selector
setOdPredicateSelector = mkSelector "setOdPredicate:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @standardAttributeTypes@
standardAttributeTypesSelector :: Selector
standardAttributeTypesSelector = mkSelector "standardAttributeTypes"

