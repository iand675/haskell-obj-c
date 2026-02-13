{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , authorizationSelector
  , initSelector
  , newSelector

  -- * Enum types
  , VSAutoSignInAuthorization(VSAutoSignInAuthorization)
  , pattern VSAutoSignInAuthorizationNotDetermined
  , pattern VSAutoSignInAuthorizationGranted
  , pattern VSAutoSignInAuthorizationDenied

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.VideoSubscriberAccount.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVSAutoSignInTokenUpdateContext vsAutoSignInTokenUpdateContext => vsAutoSignInTokenUpdateContext -> IO (Id VSAutoSignInTokenUpdateContext)
init_ vsAutoSignInTokenUpdateContext =
  sendOwnedMessage vsAutoSignInTokenUpdateContext initSelector

-- | @+ new@
new :: IO (Id VSAutoSignInTokenUpdateContext)
new  =
  do
    cls' <- getRequiredClass "VSAutoSignInTokenUpdateContext"
    sendOwnedClassMessage cls' newSelector

-- | @- authorization@
authorization :: IsVSAutoSignInTokenUpdateContext vsAutoSignInTokenUpdateContext => vsAutoSignInTokenUpdateContext -> IO VSAutoSignInAuthorization
authorization vsAutoSignInTokenUpdateContext =
  sendMessage vsAutoSignInTokenUpdateContext authorizationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VSAutoSignInTokenUpdateContext)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VSAutoSignInTokenUpdateContext)
newSelector = mkSelector "new"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector '[] VSAutoSignInAuthorization
authorizationSelector = mkSelector "authorization"

