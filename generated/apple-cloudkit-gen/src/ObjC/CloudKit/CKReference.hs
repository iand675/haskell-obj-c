{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKReference@.
module ObjC.CloudKit.CKReference
  ( CKReference
  , IsCKReference(..)
  , init_
  , new
  , initWithRecordID_action
  , initWithRecord_action
  , referenceAction
  , recordID
  , initSelector
  , initWithRecordID_actionSelector
  , initWithRecord_actionSelector
  , newSelector
  , recordIDSelector
  , referenceActionSelector

  -- * Enum types
  , CKReferenceAction(CKReferenceAction)
  , pattern CKReferenceActionNone
  , pattern CKReferenceActionDeleteSelf

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKReference ckReference => ckReference -> IO (Id CKReference)
init_ ckReference =
  sendOwnedMessage ckReference initSelector

-- | @+ new@
new :: IO (Id CKReference)
new  =
  do
    cls' <- getRequiredClass "CKReference"
    sendOwnedClassMessage cls' newSelector

-- | It is acceptable to relate two records that have not yet been uploaded to the server. Those records must be uploaded to the server in the same operation if using an action other than @CKReferenceActionNone@.
--
-- If a record references a record that does not exist on the server and is not in the current save operation it will result in an error if using an action other than @CKReferenceActionNone@.
--
-- ObjC selector: @- initWithRecordID:action:@
initWithRecordID_action :: (IsCKReference ckReference, IsCKRecordID recordID) => ckReference -> recordID -> CKReferenceAction -> IO (Id CKReference)
initWithRecordID_action ckReference recordID action =
  sendOwnedMessage ckReference initWithRecordID_actionSelector (toCKRecordID recordID) action

-- | @- initWithRecord:action:@
initWithRecord_action :: (IsCKReference ckReference, IsCKRecord record) => ckReference -> record -> CKReferenceAction -> IO (Id CKReference)
initWithRecord_action ckReference record action =
  sendOwnedMessage ckReference initWithRecord_actionSelector (toCKRecord record) action

-- | @- referenceAction@
referenceAction :: IsCKReference ckReference => ckReference -> IO CKReferenceAction
referenceAction ckReference =
  sendMessage ckReference referenceActionSelector

-- | @- recordID@
recordID :: IsCKReference ckReference => ckReference -> IO (Id CKRecordID)
recordID ckReference =
  sendMessage ckReference recordIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKReference)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKReference)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRecordID:action:@
initWithRecordID_actionSelector :: Selector '[Id CKRecordID, CKReferenceAction] (Id CKReference)
initWithRecordID_actionSelector = mkSelector "initWithRecordID:action:"

-- | @Selector@ for @initWithRecord:action:@
initWithRecord_actionSelector :: Selector '[Id CKRecord, CKReferenceAction] (Id CKReference)
initWithRecord_actionSelector = mkSelector "initWithRecord:action:"

-- | @Selector@ for @referenceAction@
referenceActionSelector :: Selector '[] CKReferenceAction
referenceActionSelector = mkSelector "referenceAction"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector '[] (Id CKRecordID)
recordIDSelector = mkSelector "recordID"

