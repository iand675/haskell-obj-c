{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLServiceSessionDiagnostic@.
module ObjC.CoreLocation.CLServiceSessionDiagnostic
  ( CLServiceSessionDiagnostic
  , IsCLServiceSessionDiagnostic(..)
  , authorizationDenied
  , authorizationDeniedGlobally
  , authorizationRestricted
  , insufficientlyInUse
  , serviceSessionRequired
  , fullAccuracyDenied
  , alwaysAuthorizationDenied
  , authorizationRequestInProgress
  , alwaysAuthorizationDeniedSelector
  , authorizationDeniedGloballySelector
  , authorizationDeniedSelector
  , authorizationRequestInProgressSelector
  , authorizationRestrictedSelector
  , fullAccuracyDeniedSelector
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
authorizationDenied :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationDenied clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic authorizationDeniedSelector

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationDeniedGlobally clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic authorizationDeniedGloballySelector

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationRestricted clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic authorizationRestrictedSelector

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
insufficientlyInUse clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic insufficientlyInUseSelector

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
serviceSessionRequired clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic serviceSessionRequiredSelector

-- | @- fullAccuracyDenied@
fullAccuracyDenied :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
fullAccuracyDenied clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic fullAccuracyDeniedSelector

-- | @- alwaysAuthorizationDenied@
alwaysAuthorizationDenied :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
alwaysAuthorizationDenied clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic alwaysAuthorizationDeniedSelector

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationRequestInProgress clServiceSessionDiagnostic =
  sendMessage clServiceSessionDiagnostic authorizationRequestInProgressSelector

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

-- | @Selector@ for @fullAccuracyDenied@
fullAccuracyDeniedSelector :: Selector '[] Bool
fullAccuracyDeniedSelector = mkSelector "fullAccuracyDenied"

-- | @Selector@ for @alwaysAuthorizationDenied@
alwaysAuthorizationDeniedSelector :: Selector '[] Bool
alwaysAuthorizationDeniedSelector = mkSelector "alwaysAuthorizationDenied"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector '[] Bool
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

