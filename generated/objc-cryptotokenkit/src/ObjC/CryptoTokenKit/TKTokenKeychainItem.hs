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
  , initWithObjectIDSelector
  , initSelector
  , objectIDSelector
  , labelSelector
  , setLabelSelector
  , constraintsSelector
  , setConstraintsSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes item with objectID.
--
-- ObjC selector: @- initWithObjectID:@
initWithObjectID :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> RawId -> IO (Id TKTokenKeychainItem)
initWithObjectID tkTokenKeychainItem  objectID =
  sendMsg tkTokenKeychainItem (mkSelector "initWithObjectID:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO (Id TKTokenKeychainItem)
init_ tkTokenKeychainItem  =
  sendMsg tkTokenKeychainItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | object ID for item identification
--
-- ObjC selector: @- objectID@
objectID :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO RawId
objectID tkTokenKeychainItem  =
  fmap (RawId . castPtr) $ sendMsg tkTokenKeychainItem (mkSelector "objectID") (retPtr retVoid) []

-- | Contains the user-visible label for this item.  This property is an equivalent of kSecAttrLabel in SecItem.h
--
-- ObjC selector: @- label@
label :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO (Id NSString)
label tkTokenKeychainItem  =
  sendMsg tkTokenKeychainItem (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Contains the user-visible label for this item.  This property is an equivalent of kSecAttrLabel in SecItem.h
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsTKTokenKeychainItem tkTokenKeychainItem, IsNSString value) => tkTokenKeychainItem -> value -> IO ()
setLabel tkTokenKeychainItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenKeychainItem (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Contains access constraints for this object keyed by TKTOpenOperation wrapped in NSNumber.
--
-- ObjC selector: @- constraints@
constraints :: IsTKTokenKeychainItem tkTokenKeychainItem => tkTokenKeychainItem -> IO (Id NSDictionary)
constraints tkTokenKeychainItem  =
  sendMsg tkTokenKeychainItem (mkSelector "constraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Contains access constraints for this object keyed by TKTOpenOperation wrapped in NSNumber.
--
-- ObjC selector: @- setConstraints:@
setConstraints :: (IsTKTokenKeychainItem tkTokenKeychainItem, IsNSDictionary value) => tkTokenKeychainItem -> value -> IO ()
setConstraints tkTokenKeychainItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenKeychainItem (mkSelector "setConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector
setConstraintsSelector = mkSelector "setConstraints:"

