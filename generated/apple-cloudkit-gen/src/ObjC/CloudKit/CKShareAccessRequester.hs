{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector
  , participantLookupInfoSelector
  , userIdentitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CKShareAccessRequester)
new  =
  do
    cls' <- getRequiredClass "CKShareAccessRequester"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsCKShareAccessRequester ckShareAccessRequester => ckShareAccessRequester -> IO (Id CKShareAccessRequester)
init_ ckShareAccessRequester =
  sendOwnedMessage ckShareAccessRequester initSelector

-- | The identity of the user requesting access to the share.
--
-- ObjC selector: @- userIdentity@
userIdentity :: IsCKShareAccessRequester ckShareAccessRequester => ckShareAccessRequester -> IO (Id CKUserIdentity)
userIdentity ckShareAccessRequester =
  sendMessage ckShareAccessRequester userIdentitySelector

-- | Lookup information for the requester.
--
-- Use this lookup info with ``CKFetchShareParticipantsOperation`` to fetch the corresponding participant. Once fetched, add the participant to the share to approve the requester.
--
-- ObjC selector: @- participantLookupInfo@
participantLookupInfo :: IsCKShareAccessRequester ckShareAccessRequester => ckShareAccessRequester -> IO (Id CKUserIdentityLookupInfo)
participantLookupInfo ckShareAccessRequester =
  sendMessage ckShareAccessRequester participantLookupInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKShareAccessRequester)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKShareAccessRequester)
initSelector = mkSelector "init"

-- | @Selector@ for @userIdentity@
userIdentitySelector :: Selector '[] (Id CKUserIdentity)
userIdentitySelector = mkSelector "userIdentity"

-- | @Selector@ for @participantLookupInfo@
participantLookupInfoSelector :: Selector '[] (Id CKUserIdentityLookupInfo)
participantLookupInfoSelector = mkSelector "participantLookupInfo"

