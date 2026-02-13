{-# LANGUAGE DataKinds #-}
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
  , certificateForObjectID_errorSelector
  , fillWithItemsSelector
  , initSelector
  , itemsSelector
  , keyForObjectID_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
fillWithItems tkTokenKeychainContents items =
  sendMessage tkTokenKeychainContents fillWithItemsSelector (toNSArray items)

-- | Returns key with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such key exists.
--
-- ObjC selector: @- keyForObjectID:error:@
keyForObjectID_error :: (IsTKTokenKeychainContents tkTokenKeychainContents, IsNSError error_) => tkTokenKeychainContents -> RawId -> error_ -> IO (Id TKTokenKeychainKey)
keyForObjectID_error tkTokenKeychainContents objectID error_ =
  sendMessage tkTokenKeychainContents keyForObjectID_errorSelector objectID (toNSError error_)

-- | Returns certificate with specified objectID.  Fills error with TKTokenErrorCodeObjectNotFound if no such certificate exists.
--
-- ObjC selector: @- certificateForObjectID:error:@
certificateForObjectID_error :: (IsTKTokenKeychainContents tkTokenKeychainContents, IsNSError error_) => tkTokenKeychainContents -> RawId -> error_ -> IO (Id TKTokenKeychainCertificate)
certificateForObjectID_error tkTokenKeychainContents objectID error_ =
  sendMessage tkTokenKeychainContents certificateForObjectID_errorSelector objectID (toNSError error_)

-- | @- init@
init_ :: IsTKTokenKeychainContents tkTokenKeychainContents => tkTokenKeychainContents -> IO (Id TKTokenKeychainContents)
init_ tkTokenKeychainContents =
  sendOwnedMessage tkTokenKeychainContents initSelector

-- | All items related to this token in the keychain.
--
-- ObjC selector: @- items@
items :: IsTKTokenKeychainContents tkTokenKeychainContents => tkTokenKeychainContents -> IO (Id NSArray)
items tkTokenKeychainContents =
  sendMessage tkTokenKeychainContents itemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fillWithItems:@
fillWithItemsSelector :: Selector '[Id NSArray] ()
fillWithItemsSelector = mkSelector "fillWithItems:"

-- | @Selector@ for @keyForObjectID:error:@
keyForObjectID_errorSelector :: Selector '[RawId, Id NSError] (Id TKTokenKeychainKey)
keyForObjectID_errorSelector = mkSelector "keyForObjectID:error:"

-- | @Selector@ for @certificateForObjectID:error:@
certificateForObjectID_errorSelector :: Selector '[RawId, Id NSError] (Id TKTokenKeychainCertificate)
certificateForObjectID_errorSelector = mkSelector "certificateForObjectID:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenKeychainContents)
initSelector = mkSelector "init"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

