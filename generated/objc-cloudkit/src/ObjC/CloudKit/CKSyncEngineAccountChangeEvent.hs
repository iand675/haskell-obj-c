{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The user signed in or out of their account.
--
-- The sync engine automatically listens for account changes, and it will send this event when the user signs in or out. It's your responsibility to react appropriately to this change and update your local persistence.
--
-- When the logged-in account changes, the sync engine will reset its internal state under the hood. This means that it will clear any pending database or record zone changes that you may have added.
--
-- Note that it's possible the account changes multiple times while your app is quit. If this happens, you will only receive one account change event representing the transition between the last known state and the current state.
--
-- Generated bindings for @CKSyncEngineAccountChangeEvent@.
module ObjC.CloudKit.CKSyncEngineAccountChangeEvent
  ( CKSyncEngineAccountChangeEvent
  , IsCKSyncEngineAccountChangeEvent(..)
  , changeType
  , previousUser
  , currentUser
  , changeTypeSelector
  , previousUserSelector
  , currentUserSelector

  -- * Enum types
  , CKSyncEngineAccountChangeType(CKSyncEngineAccountChangeType)
  , pattern CKSyncEngineAccountChangeTypeSignIn
  , pattern CKSyncEngineAccountChangeTypeSignOut
  , pattern CKSyncEngineAccountChangeTypeSwitchAccounts

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

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The type of account change that occurred.
--
-- ObjC selector: @- changeType@
changeType :: IsCKSyncEngineAccountChangeEvent ckSyncEngineAccountChangeEvent => ckSyncEngineAccountChangeEvent -> IO CKSyncEngineAccountChangeType
changeType ckSyncEngineAccountChangeEvent  =
  fmap (coerce :: CLong -> CKSyncEngineAccountChangeType) $ sendMsg ckSyncEngineAccountChangeEvent (mkSelector "changeType") retCLong []

-- | The user record ID for the previous user.
--
-- If the user just signed in, this will be @nil@. If the user signed out or switched accounts, this will be the old account.
--
-- ObjC selector: @- previousUser@
previousUser :: IsCKSyncEngineAccountChangeEvent ckSyncEngineAccountChangeEvent => ckSyncEngineAccountChangeEvent -> IO (Id CKRecordID)
previousUser ckSyncEngineAccountChangeEvent  =
  sendMsg ckSyncEngineAccountChangeEvent (mkSelector "previousUser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user record ID for the current user.
--
-- If the user just signed in or switched accounts, this will be the new user record ID. If the user signed out, this will be @nil@.
--
-- ObjC selector: @- currentUser@
currentUser :: IsCKSyncEngineAccountChangeEvent ckSyncEngineAccountChangeEvent => ckSyncEngineAccountChangeEvent -> IO (Id CKRecordID)
currentUser ckSyncEngineAccountChangeEvent  =
  sendMsg ckSyncEngineAccountChangeEvent (mkSelector "currentUser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @previousUser@
previousUserSelector :: Selector
previousUserSelector = mkSelector "previousUser"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector
currentUserSelector = mkSelector "currentUser"

