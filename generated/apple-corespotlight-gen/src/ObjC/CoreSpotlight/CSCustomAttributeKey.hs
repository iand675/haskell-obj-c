{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSCustomAttributeKey@.
module ObjC.CoreSpotlight.CSCustomAttributeKey
  ( CSCustomAttributeKey
  , IsCSCustomAttributeKey(..)
  , init_
  , initWithKeyName
  , initWithKeyName_searchable_searchableByDefault_unique_multiValued
  , keyName
  , searchable
  , searchableByDefault
  , unique
  , multiValued
  , initSelector
  , initWithKeyNameSelector
  , initWithKeyName_searchable_searchableByDefault_unique_multiValuedSelector
  , keyNameSelector
  , multiValuedSelector
  , searchableByDefaultSelector
  , searchableSelector
  , uniqueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO (Id CSCustomAttributeKey)
init_ csCustomAttributeKey =
  sendOwnedMessage csCustomAttributeKey initSelector

-- | @- initWithKeyName:@
initWithKeyName :: (IsCSCustomAttributeKey csCustomAttributeKey, IsNSString keyName) => csCustomAttributeKey -> keyName -> IO (Id CSCustomAttributeKey)
initWithKeyName csCustomAttributeKey keyName =
  sendOwnedMessage csCustomAttributeKey initWithKeyNameSelector (toNSString keyName)

-- | @- initWithKeyName:searchable:searchableByDefault:unique:multiValued:@
initWithKeyName_searchable_searchableByDefault_unique_multiValued :: (IsCSCustomAttributeKey csCustomAttributeKey, IsNSString keyName) => csCustomAttributeKey -> keyName -> Bool -> Bool -> Bool -> Bool -> IO (Id CSCustomAttributeKey)
initWithKeyName_searchable_searchableByDefault_unique_multiValued csCustomAttributeKey keyName searchable searchableByDefault unique multiValued =
  sendOwnedMessage csCustomAttributeKey initWithKeyName_searchable_searchableByDefault_unique_multiValuedSelector (toNSString keyName) searchable searchableByDefault unique multiValued

-- | @- keyName@
keyName :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO (Id NSString)
keyName csCustomAttributeKey =
  sendMessage csCustomAttributeKey keyNameSelector

-- | @- searchable@
searchable :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
searchable csCustomAttributeKey =
  sendMessage csCustomAttributeKey searchableSelector

-- | @- searchableByDefault@
searchableByDefault :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
searchableByDefault csCustomAttributeKey =
  sendMessage csCustomAttributeKey searchableByDefaultSelector

-- | @- unique@
unique :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
unique csCustomAttributeKey =
  sendMessage csCustomAttributeKey uniqueSelector

-- | @- multiValued@
multiValued :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
multiValued csCustomAttributeKey =
  sendMessage csCustomAttributeKey multiValuedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CSCustomAttributeKey)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithKeyName:@
initWithKeyNameSelector :: Selector '[Id NSString] (Id CSCustomAttributeKey)
initWithKeyNameSelector = mkSelector "initWithKeyName:"

-- | @Selector@ for @initWithKeyName:searchable:searchableByDefault:unique:multiValued:@
initWithKeyName_searchable_searchableByDefault_unique_multiValuedSelector :: Selector '[Id NSString, Bool, Bool, Bool, Bool] (Id CSCustomAttributeKey)
initWithKeyName_searchable_searchableByDefault_unique_multiValuedSelector = mkSelector "initWithKeyName:searchable:searchableByDefault:unique:multiValued:"

-- | @Selector@ for @keyName@
keyNameSelector :: Selector '[] (Id NSString)
keyNameSelector = mkSelector "keyName"

-- | @Selector@ for @searchable@
searchableSelector :: Selector '[] Bool
searchableSelector = mkSelector "searchable"

-- | @Selector@ for @searchableByDefault@
searchableByDefaultSelector :: Selector '[] Bool
searchableByDefaultSelector = mkSelector "searchableByDefault"

-- | @Selector@ for @unique@
uniqueSelector :: Selector '[] Bool
uniqueSelector = mkSelector "unique"

-- | @Selector@ for @multiValued@
multiValuedSelector :: Selector '[] Bool
multiValuedSelector = mkSelector "multiValued"

