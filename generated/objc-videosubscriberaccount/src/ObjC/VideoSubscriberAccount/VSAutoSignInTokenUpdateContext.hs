{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Context object used to update the auto sign in token. This object has to be obtained through a user consent flow using @-[VSUserAccountManager requestAutoSignInAuthorizationWithCompletionHandler:]@, then it is passed to @-[VSUserAccountManager updateAutoSignInToken:updateContext:completionHandler:]@
--
-- Generated bindings for @VSAutoSignInTokenUpdateContext@.
module ObjC.VideoSubscriberAccount.VSAutoSignInTokenUpdateContext
  ( VSAutoSignInTokenUpdateContext
  , IsVSAutoSignInTokenUpdateContext(..)
  , init_
  , new
  , authorization
  , initSelector
  , newSelector
  , authorizationSelector

  -- * Enum types
  , VSAutoSignInAuthorization(VSAutoSignInAuthorization)
  , pattern VSAutoSignInAuthorizationNotDetermined
  , pattern VSAutoSignInAuthorizationGranted
  , pattern VSAutoSignInAuthorizationDenied

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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.VideoSubscriberAccount.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVSAutoSignInTokenUpdateContext vsAutoSignInTokenUpdateContext => vsAutoSignInTokenUpdateContext -> IO (Id VSAutoSignInTokenUpdateContext)
init_ vsAutoSignInTokenUpdateContext  =
  sendMsg vsAutoSignInTokenUpdateContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VSAutoSignInTokenUpdateContext)
new  =
  do
    cls' <- getRequiredClass "VSAutoSignInTokenUpdateContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- authorization@
authorization :: IsVSAutoSignInTokenUpdateContext vsAutoSignInTokenUpdateContext => vsAutoSignInTokenUpdateContext -> IO VSAutoSignInAuthorization
authorization vsAutoSignInTokenUpdateContext  =
  fmap (coerce :: CLong -> VSAutoSignInAuthorization) $ sendMsg vsAutoSignInTokenUpdateContext (mkSelector "authorization") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector
authorizationSelector = mkSelector "authorization"

