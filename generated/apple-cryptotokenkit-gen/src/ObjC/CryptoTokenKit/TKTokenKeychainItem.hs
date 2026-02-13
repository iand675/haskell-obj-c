{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKTokenKeychainItem
--
-- Base interface for propagation token's items into the keychain.
--
-- Generated bindings for @TKTokenKeychainItem@.
module ObjC.CryptoTokenKit.TKTokenKeychainItem
  ( TKTokenKeychainItem
  , IsTKTokenKeychainItem(..)
  , initWithObjectID
  , init_
  , objectID
  , label
  , setLabel
  , constraints
  , setConstraints
  , constraintsSelector
  , initSelector
  , initWithObjectIDSelector
  , labelSelector
  , objectIDSelector
  , setConstraintsSelector
  , setLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes item with objectID.
--
-- ObjC selector: @- initWithObjectID:@
initWithObjectID :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> RawId -> IO (Id TKTokenKeychainItem)
initWithObjectID tkTokenKeychainItem objectID =
  sendOwnedMessage tkTokenKeychainItem initWithObjectIDSelector objectID

-- | @- init@
init_ :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO (Id TKTokenKeychainItem)
init_ tkTokenKeychainItem =
  sendOwnedMessage tkTokenKeychainItem initSelector

-- | object ID for item identification
--
-- ObjC selector: @- objectID@
objectID :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO RawId
objectID tkTokenKeychainItem =
  sendMessage tkTokenKeychainItem objectIDSelector

-- | Contains the user-visible label for this item.  This property is an equivalent of kSecAttrLabel in SecItem.h
--
-- ObjC selector: @- label@
label :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO (Id NSString)
label tkTokenKeychainItem =
  sendMessage tkTokenKeychainItem labelSelector

-- | Contains the user-visible label for this item.  This property is an equivalent of kSecAttrLabel in SecItem.h
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsTKTokenKeychainItem tkTokenKeychainItem, IsNSString value) => tkTokenKeychainItem -> value -> IO ()
setLabel tkTokenKeychainItem value =
  sendMessage tkTokenKeychainItem setLabelSelector (toNSString value)

-- | Contains access constraints for this object keyed by TKTOpenOperation wrapped in NSNumber.
--
-- ObjC selector: @- constraints@
constraints :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO (Id NSDictionary)
constraints tkTokenKeychainItem =
  sendMessage tkTokenKeychainItem constraintsSelector

-- | Contains access constraints for this object keyed by TKTOpenOperation wrapped in NSNumber.
--
-- ObjC selector: @- setConstraints:@
setConstraints :: (IsTKTokenKeychainItem tkTokenKeychainItem, IsNSDictionary value) => tkTokenKeychainItem -> value -> IO ()
setConstraints tkTokenKeychainItem value =
  sendMessage tkTokenKeychainItem setConstraintsSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector '[RawId] (Id TKTokenKeychainItem)
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenKeychainItem)
initSelector = mkSelector "init"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector '[] RawId
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector '[] (Id NSDictionary)
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector '[Id NSDictionary] ()
setConstraintsSelector = mkSelector "setConstraints:"

