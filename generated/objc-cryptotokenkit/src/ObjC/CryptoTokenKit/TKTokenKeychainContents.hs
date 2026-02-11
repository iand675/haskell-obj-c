{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains TKTokenKeychainItem instances (keys and certificates) which represent keychain state (i.e. set of items) of specific token.
--
-- Generated bindings for @TKTokenKeychainContents@.
module ObjC.CryptoTokenKit.TKTokenKeychainContents
  ( TKTokenKeychainContents
  , IsTKTokenKeychainContents(..)
  , fillWithItems
  , keyForObjectID_error
  , certificateForObjectID_error
  , init_
  , items
  , fillWithItemsSelector
  , keyForObjectID_errorSelector
  , certificateForObjectID_errorSelector
  , initSelector
  , itemsSelector


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

-- | Fills keychain with the set of specified items.  All items belonging to token are first removed from the keychain and then the keychain is populated with new items.
--
-- @items@ — New items to be stored into the keychain.
--
-- ObjC selector: @- fillWithItems:@
fillWithItems :: (IsTKTokenKeychainContents tkTokenKeychainContents, IsNSArray items) => tkTokenKeychainContents -> items -> IO ()
fillWithItems tkTokenKeychainContents  items =
withObjCPtr items $ \raw_items ->
    sendMsg tkTokenKeychainContents (mkSelector "fillWithItems:") retVoid [argPtr (castPtr raw_items :: Ptr ())]

-- | Returns key with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such key exists.
--
-- ObjC selector: @- keyForObjectID:error:@
keyForObjectID_error :: (IsTKTokenKeychainContents tkTokenKeychainContents, IsNSError error_) => tkTokenKeychainContents -> RawId -> error_ -> IO (Id TKTokenKeychainKey)
keyForObjectID_error tkTokenKeychainContents  objectID error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg tkTokenKeychainContents (mkSelector "keyForObjectID:error:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Returns certificate with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such certificate exists.
--
-- ObjC selector: @- certificateForObjectID:error:@
certificateForObjectID_error :: (IsTKTokenKeychainContents tkTokenKeychainContents, IsNSError error_) => tkTokenKeychainContents -> RawId -> error_ -> IO (Id TKTokenKeychainCertificate)
certificateForObjectID_error tkTokenKeychainContents  objectID error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg tkTokenKeychainContents (mkSelector "certificateForObjectID:error:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsTKTokenKeychainContents tkTokenKeychainContents => tkTokenKeychainContents -> IO (Id TKTokenKeychainContents)
init_ tkTokenKeychainContents  =
  sendMsg tkTokenKeychainContents (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | All items related to this token in the keychain.
--
-- ObjC selector: @- items@
items :: IsTKTokenKeychainContents tkTokenKeychainContents => tkTokenKeychainContents -> IO (Id NSArray)
items tkTokenKeychainContents  =
  sendMsg tkTokenKeychainContents (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fillWithItems:@
fillWithItemsSelector :: Selector
fillWithItemsSelector = mkSelector "fillWithItems:"

-- | @Selector@ for @keyForObjectID:error:@
keyForObjectID_errorSelector :: Selector
keyForObjectID_errorSelector = mkSelector "keyForObjectID:error:"

-- | @Selector@ for @certificateForObjectID:error:@
certificateForObjectID_errorSelector :: Selector
certificateForObjectID_errorSelector = mkSelector "certificateForObjectID:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

