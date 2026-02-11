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
  , authorizationDeniedSelector
  , authorizationDeniedGloballySelector
  , authorizationRestrictedSelector
  , insufficientlyInUseSelector
  , serviceSessionRequiredSelector
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
authorizationDenied :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationDenied clBackgroundActivitySessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBackgroundActivitySessionDiagnostic (mkSelector "authorizationDenied") retCULong []

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationDeniedGlobally clBackgroundActivitySessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBackgroundActivitySessionDiagnostic (mkSelector "authorizationDeniedGlobally") retCULong []

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationRestricted clBackgroundActivitySessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBackgroundActivitySessionDiagnostic (mkSelector "authorizationRestricted") retCULong []

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
insufficientlyInUse clBackgroundActivitySessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBackgroundActivitySessionDiagnostic (mkSelector "insufficientlyInUse") retCULong []

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
serviceSessionRequired clBackgroundActivitySessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBackgroundActivitySessionDiagnostic (mkSelector "serviceSessionRequired") retCULong []

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLBackgroundActivitySessionDiagnostic clBackgroundActivitySessionDiagnostic => clBackgroundActivitySessionDiagnostic -> IO Bool
authorizationRequestInProgress clBackgroundActivitySessionDiagnostic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBackgroundActivitySessionDiagnostic (mkSelector "authorizationRequestInProgress") retCULong []

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

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

