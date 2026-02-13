{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLUpdate@.
module ObjC.CoreLocation.CLUpdate
  ( CLUpdate
  , IsCLUpdate(..)
  , authorizationDenied
  , authorizationDeniedGlobally
  , authorizationRestricted
  , isStationary
  , stationary
  , insufficientlyInUse
  , locationUnavailable
  , accuracyLimited
  , serviceSessionRequired
  , authorizationRequestInProgress
  , accuracyLimitedSelector
  , authorizationDeniedGloballySelector
  , authorizationDeniedSelector
  , authorizationRequestInProgressSelector
  , authorizationRestrictedSelector
  , insufficientlyInUseSelector
  , isStationarySelector
  , locationUnavailableSelector
  , serviceSessionRequiredSelector
  , stationarySelector


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
authorizationDenied :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationDenied clUpdate =
  sendMessage clUpdate authorizationDeniedSelector

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationDeniedGlobally clUpdate =
  sendMessage clUpdate authorizationDeniedGloballySelector

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationRestricted clUpdate =
  sendMessage clUpdate authorizationRestrictedSelector

-- | @- isStationary@
isStationary :: IsCLUpdate clUpdate => clUpdate -> IO Bool
isStationary clUpdate =
  sendMessage clUpdate isStationarySelector

-- | @- stationary@
stationary :: IsCLUpdate clUpdate => clUpdate -> IO Bool
stationary clUpdate =
  sendMessage clUpdate stationarySelector

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLUpdate clUpdate => clUpdate -> IO Bool
insufficientlyInUse clUpdate =
  sendMessage clUpdate insufficientlyInUseSelector

-- | @- locationUnavailable@
locationUnavailable :: IsCLUpdate clUpdate => clUpdate -> IO Bool
locationUnavailable clUpdate =
  sendMessage clUpdate locationUnavailableSelector

-- | @- accuracyLimited@
accuracyLimited :: IsCLUpdate clUpdate => clUpdate -> IO Bool
accuracyLimited clUpdate =
  sendMessage clUpdate accuracyLimitedSelector

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLUpdate clUpdate => clUpdate -> IO Bool
serviceSessionRequired clUpdate =
  sendMessage clUpdate serviceSessionRequiredSelector

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationRequestInProgress clUpdate =
  sendMessage clUpdate authorizationRequestInProgressSelector

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

-- | @Selector@ for @isStationary@
isStationarySelector :: Selector '[] Bool
isStationarySelector = mkSelector "isStationary"

-- | @Selector@ for @stationary@
stationarySelector :: Selector '[] Bool
stationarySelector = mkSelector "stationary"

-- | @Selector@ for @insufficientlyInUse@
insufficientlyInUseSelector :: Selector '[] Bool
insufficientlyInUseSelector = mkSelector "insufficientlyInUse"

-- | @Selector@ for @locationUnavailable@
locationUnavailableSelector :: Selector '[] Bool
locationUnavailableSelector = mkSelector "locationUnavailable"

-- | @Selector@ for @accuracyLimited@
accuracyLimitedSelector :: Selector '[] Bool
accuracyLimitedSelector = mkSelector "accuracyLimited"

-- | @Selector@ for @serviceSessionRequired@
serviceSessionRequiredSelector :: Selector '[] Bool
serviceSessionRequiredSelector = mkSelector "serviceSessionRequired"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector '[] Bool
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

