{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKShareAccessRequester@.
module ObjC.CloudKit.CKShareAccessRequester
  ( CKShareAccessRequester
  , IsCKShareAccessRequester(..)
  , new
  , init_
  , userIdentity
  , participantLookupInfo
  , newSelector
  , initSelector
  , userIdentitySelector
  , participantLookupInfoSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CKShareAccessRequester)
new  =
  do
    cls' <- getRequiredClass "CKShareAccessRequester"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCKShareAccessRequester ckShareAccessRequester => ckShareAccessRequester -> IO (Id CKShareAccessRequester)
init_ ckShareAccessRequester  =
  sendMsg ckShareAccessRequester (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The identity of the user requesting access to the share.
--
-- ObjC selector: @- userIdentity@
userIdentity :: IsCKShareAccessRequester ckShareAccessRequester => ckShareAccessRequester -> IO (Id CKUserIdentity)
userIdentity ckShareAccessRequester  =
  sendMsg ckShareAccessRequester (mkSelector "userIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Lookup information for the requester.
--
-- Use this lookup info with ``CKFetchShareParticipantsOperation`` to fetch the corresponding participant. Once fetched, add the participant to the share to approve the requester.
--
-- ObjC selector: @- participantLookupInfo@
participantLookupInfo :: IsCKShareAccessRequester ckShareAccessRequester => ckShareAccessRequester -> IO (Id CKUserIdentityLookupInfo)
participantLookupInfo ckShareAccessRequester  =
  sendMsg ckShareAccessRequester (mkSelector "participantLookupInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @userIdentity@
userIdentitySelector :: Selector
userIdentitySelector = mkSelector "userIdentity"

-- | @Selector@ for @participantLookupInfo@
participantLookupInfoSelector :: Selector
participantLookupInfoSelector = mkSelector "participantLookupInfo"

