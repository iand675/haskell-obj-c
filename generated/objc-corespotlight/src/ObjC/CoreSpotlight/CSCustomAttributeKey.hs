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
  , searchableSelector
  , searchableByDefaultSelector
  , uniqueSelector
  , multiValuedSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO (Id CSCustomAttributeKey)
init_ csCustomAttributeKey  =
  sendMsg csCustomAttributeKey (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithKeyName:@
initWithKeyName :: (IsCSCustomAttributeKey csCustomAttributeKey, IsNSString keyName) => csCustomAttributeKey -> keyName -> IO (Id CSCustomAttributeKey)
initWithKeyName csCustomAttributeKey  keyName =
withObjCPtr keyName $ \raw_keyName ->
    sendMsg csCustomAttributeKey (mkSelector "initWithKeyName:") (retPtr retVoid) [argPtr (castPtr raw_keyName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithKeyName:searchable:searchableByDefault:unique:multiValued:@
initWithKeyName_searchable_searchableByDefault_unique_multiValued :: (IsCSCustomAttributeKey csCustomAttributeKey, IsNSString keyName) => csCustomAttributeKey -> keyName -> Bool -> Bool -> Bool -> Bool -> IO (Id CSCustomAttributeKey)
initWithKeyName_searchable_searchableByDefault_unique_multiValued csCustomAttributeKey  keyName searchable searchableByDefault unique multiValued =
withObjCPtr keyName $ \raw_keyName ->
    sendMsg csCustomAttributeKey (mkSelector "initWithKeyName:searchable:searchableByDefault:unique:multiValued:") (retPtr retVoid) [argPtr (castPtr raw_keyName :: Ptr ()), argCULong (if searchable then 1 else 0), argCULong (if searchableByDefault then 1 else 0), argCULong (if unique then 1 else 0), argCULong (if multiValued then 1 else 0)] >>= ownedObject . castPtr

-- | @- keyName@
keyName :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO (Id NSString)
keyName csCustomAttributeKey  =
  sendMsg csCustomAttributeKey (mkSelector "keyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- searchable@
searchable :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
searchable csCustomAttributeKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csCustomAttributeKey (mkSelector "searchable") retCULong []

-- | @- searchableByDefault@
searchableByDefault :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
searchableByDefault csCustomAttributeKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csCustomAttributeKey (mkSelector "searchableByDefault") retCULong []

-- | @- unique@
unique :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
unique csCustomAttributeKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csCustomAttributeKey (mkSelector "unique") retCULong []

-- | @- multiValued@
multiValued :: IsCSCustomAttributeKey csCustomAttributeKey => csCustomAttributeKey -> IO Bool
multiValued csCustomAttributeKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csCustomAttributeKey (mkSelector "multiValued") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithKeyName:@
initWithKeyNameSelector :: Selector
initWithKeyNameSelector = mkSelector "initWithKeyName:"

-- | @Selector@ for @initWithKeyName:searchable:searchableByDefault:unique:multiValued:@
initWithKeyName_searchable_searchableByDefault_unique_multiValuedSelector :: Selector
initWithKeyName_searchable_searchableByDefault_unique_multiValuedSelector = mkSelector "initWithKeyName:searchable:searchableByDefault:unique:multiValued:"

-- | @Selector@ for @keyName@
keyNameSelector :: Selector
keyNameSelector = mkSelector "keyName"

-- | @Selector@ for @searchable@
searchableSelector :: Selector
searchableSelector = mkSelector "searchable"

-- | @Selector@ for @searchableByDefault@
searchableByDefaultSelector :: Selector
searchableByDefaultSelector = mkSelector "searchableByDefault"

-- | @Selector@ for @unique@
uniqueSelector :: Selector
uniqueSelector = mkSelector "unique"

-- | @Selector@ for @multiValued@
multiValuedSelector :: Selector
multiValuedSelector = mkSelector "multiValued"

