{-# LANGUAGE PatternSynonyms #-}
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
  , requestAuthorizationWithCompletionHandlerSelector
  , defaultCenterSelector
  , focusStatusSelector
  , authorizationStatusSelector

  -- * Enum types
  , INFocusStatusAuthorizationStatus(INFocusStatusAuthorizationStatus)
  , pattern INFocusStatusAuthorizationStatusNotDetermined
  , pattern INFocusStatusAuthorizationStatusRestricted
  , pattern INFocusStatusAuthorizationStatusDenied
  , pattern INFocusStatusAuthorizationStatusAuthorized

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandler :: IsINFocusStatusCenter inFocusStatusCenter => inFocusStatusCenter -> Ptr () -> IO ()
requestAuthorizationWithCompletionHandler inFocusStatusCenter  completionHandler =
  sendMsg inFocusStatusCenter (mkSelector "requestAuthorizationWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ defaultCenter@
defaultCenter :: IO (Id INFocusStatusCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "INFocusStatusCenter"
    sendClassMsg cls' (mkSelector "defaultCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- focusStatus@
focusStatus :: IsINFocusStatusCenter inFocusStatusCenter => inFocusStatusCenter -> IO (Id INFocusStatus)
focusStatus inFocusStatusCenter  =
  sendMsg inFocusStatusCenter (mkSelector "focusStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- authorizationStatus@
authorizationStatus :: IsINFocusStatusCenter inFocusStatusCenter => inFocusStatusCenter -> IO INFocusStatusAuthorizationStatus
authorizationStatus inFocusStatusCenter  =
  fmap (coerce :: CLong -> INFocusStatusAuthorizationStatus) $ sendMsg inFocusStatusCenter (mkSelector "authorizationStatus") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandlerSelector :: Selector
requestAuthorizationWithCompletionHandlerSelector = mkSelector "requestAuthorizationWithCompletionHandler:"

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector
defaultCenterSelector = mkSelector "defaultCenter"

-- | @Selector@ for @focusStatus@
focusStatusSelector :: Selector
focusStatusSelector = mkSelector "focusStatus"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

