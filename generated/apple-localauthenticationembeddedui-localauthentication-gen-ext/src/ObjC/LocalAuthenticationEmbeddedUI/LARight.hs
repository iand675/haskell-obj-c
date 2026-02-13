{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
authorizeWithLocalizedReason_inPresentationContext_completion :: (IsLARight laRight, IsNSString localizedReason) => laRight -> localizedReason -> RawId -> Ptr () -> IO ()
authorizeWithLocalizedReason_inPresentationContext_completion laRight localizedReason presentationContext handler =
  sendMessage laRight authorizeWithLocalizedReason_inPresentationContext_completionSelector (toNSString localizedReason) presentationContext handler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizeWithLocalizedReason:inPresentationContext:completion:@
authorizeWithLocalizedReason_inPresentationContext_completionSelector :: Selector '[Id NSString, RawId, Ptr ()] ()
authorizeWithLocalizedReason_inPresentationContext_completionSelector = mkSelector "authorizeWithLocalizedReason:inPresentationContext:completion:"

