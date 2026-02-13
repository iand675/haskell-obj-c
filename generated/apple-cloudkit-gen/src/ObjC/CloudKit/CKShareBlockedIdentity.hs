{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKShareBlockedIdentity@.
module ObjC.CloudKit.CKShareBlockedIdentity
  ( CKShareBlockedIdentity
  , IsCKShareBlockedIdentity(..)
  , new
  , init_
  , userIdentity
  , initSelector
  , newSelector
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
new :: IO (Id CKShareBlockedIdentity)
new  =
  do
    cls' <- getRequiredClass "CKShareBlockedIdentity"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsCKShareBlockedIdentity ckShareBlockedIdentity => ckShareBlockedIdentity -> IO (Id CKShareBlockedIdentity)
init_ ckShareBlockedIdentity =
  sendOwnedMessage ckShareBlockedIdentity initSelector

-- | The identity of the user who has been blocked from requesting access to the share.
--
-- ObjC selector: @- userIdentity@
userIdentity :: IsCKShareBlockedIdentity ckShareBlockedIdentity => ckShareBlockedIdentity -> IO (Id CKUserIdentity)
userIdentity ckShareBlockedIdentity =
  sendMessage ckShareBlockedIdentity userIdentitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKShareBlockedIdentity)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKShareBlockedIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @userIdentity@
userIdentitySelector :: Selector '[] (Id CKUserIdentity)
userIdentitySelector = mkSelector "userIdentity"

