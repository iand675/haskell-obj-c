{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initWithRecordID_actionSelector
  , initWithRecord_actionSelector
  , referenceActionSelector
  , recordIDSelector

  -- * Enum types
  , CKReferenceAction(CKReferenceAction)
  , pattern CKReferenceActionNone
  , pattern CKReferenceActionDeleteSelf

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

-- | @- init@
init_ :: IsCKReference ckReference => ckReference -> IO (Id CKReference)
init_ ckReference  =
  sendMsg ckReference (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKReference)
new  =
  do
    cls' <- getRequiredClass "CKReference"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | It is acceptable to relate two records that have not yet been uploaded to the server. Those records must be uploaded to the server in the same operation if using an action other than @CKReferenceActionNone@.
--
-- If a record references a record that does not exist on the server and is not in the current save operation it will result in an error if using an action other than @CKReferenceActionNone@.
--
-- ObjC selector: @- initWithRecordID:action:@
initWithRecordID_action :: (IsCKReference ckReference, IsCKRecordID recordID) => ckReference -> recordID -> CKReferenceAction -> IO (Id CKReference)
initWithRecordID_action ckReference  recordID action =
withObjCPtr recordID $ \raw_recordID ->
    sendMsg ckReference (mkSelector "initWithRecordID:action:") (retPtr retVoid) [argPtr (castPtr raw_recordID :: Ptr ()), argCULong (coerce action)] >>= ownedObject . castPtr

-- | @- initWithRecord:action:@
initWithRecord_action :: (IsCKReference ckReference, IsCKRecord record) => ckReference -> record -> CKReferenceAction -> IO (Id CKReference)
initWithRecord_action ckReference  record action =
withObjCPtr record $ \raw_record ->
    sendMsg ckReference (mkSelector "initWithRecord:action:") (retPtr retVoid) [argPtr (castPtr raw_record :: Ptr ()), argCULong (coerce action)] >>= ownedObject . castPtr

-- | @- referenceAction@
referenceAction :: IsCKReference ckReference => ckReference -> IO CKReferenceAction
referenceAction ckReference  =
  fmap (coerce :: CULong -> CKReferenceAction) $ sendMsg ckReference (mkSelector "referenceAction") retCULong []

-- | @- recordID@
recordID :: IsCKReference ckReference => ckReference -> IO (Id CKRecordID)
recordID ckReference  =
  sendMsg ckReference (mkSelector "recordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRecordID:action:@
initWithRecordID_actionSelector :: Selector
initWithRecordID_actionSelector = mkSelector "initWithRecordID:action:"

-- | @Selector@ for @initWithRecord:action:@
initWithRecord_actionSelector :: Selector
initWithRecord_actionSelector = mkSelector "initWithRecord:action:"

-- | @Selector@ for @referenceAction@
referenceActionSelector :: Selector
referenceActionSelector = mkSelector "referenceAction"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector
recordIDSelector = mkSelector "recordID"

