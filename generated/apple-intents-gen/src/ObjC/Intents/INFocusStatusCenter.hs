{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFocusStatusCenter@.
module ObjC.Intents.INFocusStatusCenter
  ( INFocusStatusCenter
  , IsINFocusStatusCenter(..)
  , requestAuthorizationWithCompletionHandler
  , defaultCenter
  , focusStatus
  , authorizationStatus
  , authorizationStatusSelector
  , defaultCenterSelector
  , focusStatusSelector
  , requestAuthorizationWithCompletionHandlerSelector

  -- * Enum types
  , INFocusStatusAuthorizationStatus(INFocusStatusAuthorizationStatus)
  , pattern INFocusStatusAuthorizationStatusNotDetermined
  , pattern INFocusStatusAuthorizationStatusRestricted
  , pattern INFocusStatusAuthorizationStatusDenied
  , pattern INFocusStatusAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandler :: IsINFocusStatusCenter inFocusStatusCenter => inFocusStatusCenter -> Ptr () -> IO ()
requestAuthorizationWithCompletionHandler inFocusStatusCenter completionHandler =
  sendMessage inFocusStatusCenter requestAuthorizationWithCompletionHandlerSelector completionHandler

-- | @+ defaultCenter@
defaultCenter :: IO (Id INFocusStatusCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "INFocusStatusCenter"
    sendClassMessage cls' defaultCenterSelector

-- | @- focusStatus@
focusStatus :: IsINFocusStatusCenter inFocusStatusCenter => inFocusStatusCenter -> IO (Id INFocusStatus)
focusStatus inFocusStatusCenter =
  sendMessage inFocusStatusCenter focusStatusSelector

-- | @- authorizationStatus@
authorizationStatus :: IsINFocusStatusCenter inFocusStatusCenter => inFocusStatusCenter -> IO INFocusStatusAuthorizationStatus
authorizationStatus inFocusStatusCenter =
  sendMessage inFocusStatusCenter authorizationStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestAuthorizationWithCompletionHandlerSelector = mkSelector "requestAuthorizationWithCompletionHandler:"

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector '[] (Id INFocusStatusCenter)
defaultCenterSelector = mkSelector "defaultCenter"

-- | @Selector@ for @focusStatus@
focusStatusSelector :: Selector '[] (Id INFocusStatus)
focusStatusSelector = mkSelector "focusStatus"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] INFocusStatusAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

