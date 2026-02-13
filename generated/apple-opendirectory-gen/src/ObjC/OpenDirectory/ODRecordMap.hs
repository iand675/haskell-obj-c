{-# LANGUAGE DataKinds #-}
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
  , attributeMapForStandardAttributeSelector
  , attributesSelector
  , nativeSelector
  , odPredicateSelector
  , recordMapSelector
  , setAttributeMap_forStandardAttributeSelector
  , setNativeSelector
  , setOdPredicateSelector
  , standardAttributeTypesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' recordMapSelector

-- | attributeMapForStandardAttribute:
--
-- Returns an ODAttributeMap object for the given OD standard attribute.
--
-- Returns an ODAttributeMap object for the given OD standard attribute.
--
-- ObjC selector: @- attributeMapForStandardAttribute:@
attributeMapForStandardAttribute :: (IsODRecordMap odRecordMap, IsNSString standardAttribute) => odRecordMap -> standardAttribute -> IO (Id ODAttributeMap)
attributeMapForStandardAttribute odRecordMap standardAttribute =
  sendMessage odRecordMap attributeMapForStandardAttributeSelector (toNSString standardAttribute)

-- | setAttributeMap:forStandardAttribute:
--
-- Sets an ODAttributeMap object for a given OD standard attribute.
--
-- Sets an ODAttributeMap object for a given OD standard attribute.
--
-- ObjC selector: @- setAttributeMap:forStandardAttribute:@
setAttributeMap_forStandardAttribute :: (IsODRecordMap odRecordMap, IsODAttributeMap attributeMap, IsNSString standardAttribute) => odRecordMap -> attributeMap -> standardAttribute -> IO ()
setAttributeMap_forStandardAttribute odRecordMap attributeMap standardAttribute =
  sendMessage odRecordMap setAttributeMap_forStandardAttributeSelector (toODAttributeMap attributeMap) (toNSString standardAttribute)

-- | @- native@
native :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSString)
native odRecordMap =
  sendMessage odRecordMap nativeSelector

-- | @- setNative:@
setNative :: (IsODRecordMap odRecordMap, IsNSString value) => odRecordMap -> value -> IO ()
setNative odRecordMap value =
  sendMessage odRecordMap setNativeSelector (toNSString value)

-- | @- odPredicate@
odPredicate :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSDictionary)
odPredicate odRecordMap =
  sendMessage odRecordMap odPredicateSelector

-- | @- setOdPredicate:@
setOdPredicate :: (IsODRecordMap odRecordMap, IsNSDictionary value) => odRecordMap -> value -> IO ()
setOdPredicate odRecordMap value =
  sendMessage odRecordMap setOdPredicateSelector (toNSDictionary value)

-- | @- attributes@
attributes :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSDictionary)
attributes odRecordMap =
  sendMessage odRecordMap attributesSelector

-- | @- standardAttributeTypes@
standardAttributeTypes :: IsODRecordMap odRecordMap => odRecordMap -> IO (Id NSArray)
standardAttributeTypes odRecordMap =
  sendMessage odRecordMap standardAttributeTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recordMap@
recordMapSelector :: Selector '[] (Id ODRecordMap)
recordMapSelector = mkSelector "recordMap"

-- | @Selector@ for @attributeMapForStandardAttribute:@
attributeMapForStandardAttributeSelector :: Selector '[Id NSString] (Id ODAttributeMap)
attributeMapForStandardAttributeSelector = mkSelector "attributeMapForStandardAttribute:"

-- | @Selector@ for @setAttributeMap:forStandardAttribute:@
setAttributeMap_forStandardAttributeSelector :: Selector '[Id ODAttributeMap, Id NSString] ()
setAttributeMap_forStandardAttributeSelector = mkSelector "setAttributeMap:forStandardAttribute:"

-- | @Selector@ for @native@
nativeSelector :: Selector '[] (Id NSString)
nativeSelector = mkSelector "native"

-- | @Selector@ for @setNative:@
setNativeSelector :: Selector '[Id NSString] ()
setNativeSelector = mkSelector "setNative:"

-- | @Selector@ for @odPredicate@
odPredicateSelector :: Selector '[] (Id NSDictionary)
odPredicateSelector = mkSelector "odPredicate"

-- | @Selector@ for @setOdPredicate:@
setOdPredicateSelector :: Selector '[Id NSDictionary] ()
setOdPredicateSelector = mkSelector "setOdPredicate:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @standardAttributeTypes@
standardAttributeTypesSelector :: Selector '[] (Id NSArray)
standardAttributeTypesSelector = mkSelector "standardAttributeTypes"

