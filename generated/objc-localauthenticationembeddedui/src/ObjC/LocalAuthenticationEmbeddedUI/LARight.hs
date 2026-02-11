{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups a set of requirements that need to be satisfied in order to grant access to certain resource or operation
--
-- Generated bindings for @LARight@.
module ObjC.LocalAuthenticationEmbeddedUI.LARight
  ( LARight
  , IsLARight(..)
  , authorizeWithLocalizedReason_inPresentationContext_completion
  , authorizeWithLocalizedReason_inPresentationContext_completionSelector


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

import ObjC.LocalAuthenticationEmbeddedUI.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.LocalAuthentication.Internal.Classes

-- | Tries to authorize the right.
--
-- @localizedReason@ — Localized explanation for the authorization. Appears in the UI presented to the user.
--
-- @presentationContext@ — Container where the authorization UI will be presented.
--
-- @handler@ — Completion handler called after the authorization finishses. Returns an error when the authorization fails.
--
-- ObjC selector: @- authorizeWithLocalizedReason:inPresentationContext:completion:@
authorizeWithLocalizedReason_inPresentationContext_completion :: (IsLARight laRight, IsNSString localizedReason, IsNSWindow presentationContext) => laRight -> localizedReason -> presentationContext -> Ptr () -> IO ()
authorizeWithLocalizedReason_inPresentationContext_completion laRight  localizedReason presentationContext handler =
withObjCPtr localizedReason $ \raw_localizedReason ->
  withObjCPtr presentationContext $ \raw_presentationContext ->
      sendMsg laRight (mkSelector "authorizeWithLocalizedReason:inPresentationContext:completion:") retVoid [argPtr (castPtr raw_localizedReason :: Ptr ()), argPtr (castPtr raw_presentationContext :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizeWithLocalizedReason:inPresentationContext:completion:@
authorizeWithLocalizedReason_inPresentationContext_completionSelector :: Selector
authorizeWithLocalizedReason_inPresentationContext_completionSelector = mkSelector "authorizeWithLocalizedReason:inPresentationContext:completion:"

