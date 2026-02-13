{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of an attribute implemented on a server cluster by an MTRDeviceController.  An attribute has an identifier and a value, and may or may not be writable.
--
-- MTRServerAttribute's API can be accessed from any thread.
--
-- Generated bindings for @MTRServerAttribute@.
module ObjC.Matter.MTRServerAttribute
  ( MTRServerAttribute
  , IsMTRServerAttribute(..)
  , init_
  , new
  , initReadonlyAttributeWithID_initialValue_requiredPrivilege
  , setValue
  , newFeatureMapAttributeWithInitialValue
  , attributeID
  , value
  , requiredReadPrivilege
  , writable
  , attributeIDSelector
  , initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector
  , initSelector
  , newFeatureMapAttributeWithInitialValueSelector
  , newSelector
  , requiredReadPrivilegeSelector
  , setValueSelector
  , valueSelector
  , writableSelector

  -- * Enum types
  , MTRAccessControlEntryPrivilege(MTRAccessControlEntryPrivilege)
  , pattern MTRAccessControlEntryPrivilegeView
  , pattern MTRAccessControlEntryPrivilegeProxyView
  , pattern MTRAccessControlEntryPrivilegeOperate
  , pattern MTRAccessControlEntryPrivilegeManage
  , pattern MTRAccessControlEntryPrivilegeAdminister

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO (Id MTRServerAttribute)
init_ mtrServerAttribute =
  sendOwnedMessage mtrServerAttribute initSelector

-- | @+ new@
new :: IO (Id MTRServerAttribute)
new  =
  do
    cls' <- getRequiredClass "MTRServerAttribute"
    sendOwnedClassMessage cls' newSelector

-- | Initialize as a readonly attribute.  The value is a data-value as documented in MTRBaseDevice.h.
--
-- Will fail if the attribute ID is not valid per the Matter specification or the attribute value is not a valid data-value.
--
-- requiredPrivilege is the privilege required to read the attribute. This initializer may fail if the provided attributeID is a global attribute and the provided requiredPrivilege value is not correct for that attribute ID.
--
-- ObjC selector: @- initReadonlyAttributeWithID:initialValue:requiredPrivilege:@
initReadonlyAttributeWithID_initialValue_requiredPrivilege :: (IsMTRServerAttribute mtrServerAttribute, IsNSNumber attributeID, IsNSDictionary value) => mtrServerAttribute -> attributeID -> value -> MTRAccessControlEntryPrivilege -> IO (Id MTRServerAttribute)
initReadonlyAttributeWithID_initialValue_requiredPrivilege mtrServerAttribute attributeID value requiredPrivilege =
  sendOwnedMessage mtrServerAttribute initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector (toNSNumber attributeID) (toNSDictionary value) requiredPrivilege

-- | Change the value of the attribute to a new value.  The value is a data-value as documented in MTRBaseDevice.h.
--
-- Will fail if the attribute is not a valid data-value.
--
-- ObjC selector: @- setValue:@
setValue :: (IsMTRServerAttribute mtrServerAttribute, IsNSDictionary value) => mtrServerAttribute -> value -> IO Bool
setValue mtrServerAttribute value =
  sendMessage mtrServerAttribute setValueSelector (toNSDictionary value)

-- | Create an attribute description for a FeatureMap attribute with the provided value (expected to be an unsigned integer representing the value of the bitmap). This will automatically set requiredPrivilege to the right value for FeatureMap.
--
-- ObjC selector: @+ newFeatureMapAttributeWithInitialValue:@
newFeatureMapAttributeWithInitialValue :: IsNSNumber value => value -> IO (Id MTRServerAttribute)
newFeatureMapAttributeWithInitialValue value =
  do
    cls' <- getRequiredClass "MTRServerAttribute"
    sendOwnedClassMessage cls' newFeatureMapAttributeWithInitialValueSelector (toNSNumber value)

-- | @- attributeID@
attributeID :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO (Id NSNumber)
attributeID mtrServerAttribute =
  sendMessage mtrServerAttribute attributeIDSelector

-- | @- value@
value :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO (Id NSDictionary)
value mtrServerAttribute =
  sendMessage mtrServerAttribute valueSelector

-- | The privilege level necessary to read this attribute.
--
-- ObjC selector: @- requiredReadPrivilege@
requiredReadPrivilege :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO MTRAccessControlEntryPrivilege
requiredReadPrivilege mtrServerAttribute =
  sendMessage mtrServerAttribute requiredReadPrivilegeSelector

-- | @- writable@
writable :: IsMTRServerAttribute mtrServerAttribute => mtrServerAttribute -> IO Bool
writable mtrServerAttribute =
  sendMessage mtrServerAttribute writableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRServerAttribute)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRServerAttribute)
newSelector = mkSelector "new"

-- | @Selector@ for @initReadonlyAttributeWithID:initialValue:requiredPrivilege:@
initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector :: Selector '[Id NSNumber, Id NSDictionary, MTRAccessControlEntryPrivilege] (Id MTRServerAttribute)
initReadonlyAttributeWithID_initialValue_requiredPrivilegeSelector = mkSelector "initReadonlyAttributeWithID:initialValue:requiredPrivilege:"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSDictionary] Bool
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @newFeatureMapAttributeWithInitialValue:@
newFeatureMapAttributeWithInitialValueSelector :: Selector '[Id NSNumber] (Id MTRServerAttribute)
newFeatureMapAttributeWithInitialValueSelector = mkSelector "newFeatureMapAttributeWithInitialValue:"

-- | @Selector@ for @attributeID@
attributeIDSelector :: Selector '[] (Id NSNumber)
attributeIDSelector = mkSelector "attributeID"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSDictionary)
valueSelector = mkSelector "value"

-- | @Selector@ for @requiredReadPrivilege@
requiredReadPrivilegeSelector :: Selector '[] MTRAccessControlEntryPrivilege
requiredReadPrivilegeSelector = mkSelector "requiredReadPrivilege"

-- | @Selector@ for @writable@
writableSelector :: Selector '[] Bool
writableSelector = mkSelector "writable"

