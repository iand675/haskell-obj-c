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
  , authorizationDeniedSelector
  , authorizationDeniedGloballySelector
  , authorizationRestrictedSelector
  , insufficientlyInUseSelector
  , serviceSessionRequiredSelector
  , fullAccuracyDeniedSelector
  , alwaysAuthorizationDeniedSelector
  , authorizationRequestInProgressSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- authorizationDenied@
authorizationDenied :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationDenied clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "authorizationDenied") retCULong []

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationDeniedGlobally clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "authorizationDeniedGlobally") retCULong []

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationRestricted clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "authorizationRestricted") retCULong []

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
insufficientlyInUse clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "insufficientlyInUse") retCULong []

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
serviceSessionRequired clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "serviceSessionRequired") retCULong []

-- | @- fullAccuracyDenied@
fullAccuracyDenied :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
fullAccuracyDenied clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "fullAccuracyDenied") retCULong []

-- | @- alwaysAuthorizationDenied@
alwaysAuthorizationDenied :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
alwaysAuthorizationDenied clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "alwaysAuthorizationDenied") retCULong []

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLServiceSessionDiagnostic clServiceSessionDiagnostic => clServiceSessionDiagnostic -> IO Bool
authorizationRequestInProgress clServiceSessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clServiceSessionDiagnostic (mkSelector "authorizationRequestInProgress") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationDenied@
authorizationDeniedSelector :: Selector
authorizationDeniedSelector = mkSelector "authorizationDenied"

-- | @Selector@ for @authorizationDeniedGlobally@
authorizationDeniedGloballySelector :: Selector
authorizationDeniedGloballySelector = mkSelector "authorizationDeniedGlobally"

-- | @Selector@ for @authorizationRestricted@
authorizationRestrictedSelector :: Selector
authorizationRestrictedSelector = mkSelector "authorizationRestricted"

-- | @Selector@ for @insufficientlyInUse@
insufficientlyInUseSelector :: Selector
insufficientlyInUseSelector = mkSelector "insufficientlyInUse"

-- | @Selector@ for @serviceSessionRequired@
serviceSessionRequiredSelector :: Selector
serviceSessionRequiredSelector = mkSelector "serviceSessionRequired"

-- | @Selector@ for @fullAccuracyDenied@
fullAccuracyDeniedSelector :: Selector
fullAccuracyDeniedSelector = mkSelector "fullAccuracyDenied"

-- | @Selector@ for @alwaysAuthorizationDenied@
alwaysAuthorizationDeniedSelector :: Selector
alwaysAuthorizationDeniedSelector = mkSelector "alwaysAuthorizationDenied"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

