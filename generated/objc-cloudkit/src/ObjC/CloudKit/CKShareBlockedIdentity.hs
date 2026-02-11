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
  , newSelector
  , initSelector
  , userIdentitySelector


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
new :: IO (Id CKShareBlockedIdentity)
new  =
  do
    cls' <- getRequiredClass "CKShareBlockedIdentity"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCKShareBlockedIdentity ckShareBlockedIdentity => ckShareBlockedIdentity -> IO (Id CKShareBlockedIdentity)
init_ ckShareBlockedIdentity  =
  sendMsg ckShareBlockedIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The identity of the user who has been blocked from requesting access to the share.
--
-- ObjC selector: @- userIdentity@
userIdentity :: IsCKShareBlockedIdentity ckShareBlockedIdentity => ckShareBlockedIdentity -> IO (Id CKUserIdentity)
userIdentity ckShareBlockedIdentity  =
  sendMsg ckShareBlockedIdentity (mkSelector "userIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

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

