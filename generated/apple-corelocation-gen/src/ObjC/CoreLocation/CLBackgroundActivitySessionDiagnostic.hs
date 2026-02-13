{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLBackgroundActivitySessionDiagnostic@.
module ObjC.CoreLocation.CLBackgroundActivitySessionDiagnostic
  ( CLBackgroundActivitySessionDiagnostic
  , IsCLBackgroundActivitySessionDiagnostic(..)
  , authorizationDenied
  , authorizationDeniedGlobally
  , authorizationRestricted
  , insufficientlyInUse
  , serviceSessionRequired
  , authorizationRequestInProgress
  , authorizationDeniedGloballySelector
  , authorizationDeniedSelector
  , authorizationRequestInProgressSelector
  , authorizationRestrictedSelector
  , insufficientlyInUseSelector
  , serviceSessionRequiredSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- authorizationDenied@
authorizationDenied :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationDenied clBackgroundActivitySessionDiagnostic =
  sendMessage clBackgroundActivitySessionDiagnostic authorizationDeniedSelector

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationDeniedGlobally clBackgroundActivitySessionDiagnostic =
  sendMessage clBackgroundActivitySessionDiagnostic authorizationDeniedGloballySelector

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationRestricted clBackgroundActivitySessionDiagnostic =
  sendMessage clBackgroundActivitySessionDiagnostic authorizationRestrictedSelector

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
insufficientlyInUse clBackgroundActivitySessionDiagnostic =
  sendMessage clBackgroundActivitySessionDiagnostic insufficientlyInUseSelector

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
serviceSessionRequired clBackgroundActivitySessionDiagnostic =
  sendMessage clBackgroundActivitySessionDiagnostic serviceSessionRequiredSelector

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationRequestInProgress clBackgroundActivitySessionDiagnostic =
  sendMessage clBackgroundActivitySessionDiagnostic authorizationRequestInProgressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationDenied@
authorizationDeniedSelector :: Selector '[] Bool
authorizationDeniedSelector = mkSelector "authorizationDenied"

-- | @Selector@ for @authorizationDeniedGlobally@
authorizationDeniedGloballySelector :: Selector '[] Bool
authorizationDeniedGloballySelector = mkSelector "authorizationDeniedGlobally"

-- | @Selector@ for @authorizationRestricted@
authorizationRestrictedSelector :: Selector '[] Bool
authorizationRestrictedSelector = mkSelector "authorizationRestricted"

-- | @Selector@ for @insufficientlyInUse@
insufficientlyInUseSelector :: Selector '[] Bool
insufficientlyInUseSelector = mkSelector "insufficientlyInUse"

-- | @Selector@ for @serviceSessionRequired@
serviceSessionRequiredSelector :: Selector '[] Bool
serviceSessionRequiredSelector = mkSelector "serviceSessionRequired"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector '[] Bool
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

