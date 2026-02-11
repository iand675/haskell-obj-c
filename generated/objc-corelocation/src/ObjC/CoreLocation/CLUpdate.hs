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
  , location
  , authorizationDeniedSelector
  , authorizationDeniedGloballySelector
  , authorizationRestrictedSelector
  , isStationarySelector
  , stationarySelector
  , insufficientlyInUseSelector
  , locationUnavailableSelector
  , accuracyLimitedSelector
  , serviceSessionRequiredSelector
  , authorizationRequestInProgressSelector
  , locationSelector


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
authorizationDenied :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationDenied clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "authorizationDenied") retCULong []

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationDeniedGlobally clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "authorizationDeniedGlobally") retCULong []

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationRestricted clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "authorizationRestricted") retCULong []

-- | @- isStationary@
isStationary :: IsCLUpdate clUpdate => clUpdate -> IO Bool
isStationary clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "isStationary") retCULong []

-- | @- stationary@
stationary :: IsCLUpdate clUpdate => clUpdate -> IO Bool
stationary clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "stationary") retCULong []

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLUpdate clUpdate => clUpdate -> IO Bool
insufficientlyInUse clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "insufficientlyInUse") retCULong []

-- | @- locationUnavailable@
locationUnavailable :: IsCLUpdate clUpdate => clUpdate -> IO Bool
locationUnavailable clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "locationUnavailable") retCULong []

-- | @- accuracyLimited@
accuracyLimited :: IsCLUpdate clUpdate => clUpdate -> IO Bool
accuracyLimited clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "accuracyLimited") retCULong []

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLUpdate clUpdate => clUpdate -> IO Bool
serviceSessionRequired clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "serviceSessionRequired") retCULong []

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLUpdate clUpdate => clUpdate -> IO Bool
authorizationRequestInProgress clUpdate  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clUpdate (mkSelector "authorizationRequestInProgress") retCULong []

-- | @- location@
location :: IsCLUpdate clUpdate => clUpdate -> IO (Id CLLocation)
location clUpdate  =
  sendMsg clUpdate (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @isStationary@
isStationarySelector :: Selector
isStationarySelector = mkSelector "isStationary"

-- | @Selector@ for @stationary@
stationarySelector :: Selector
stationarySelector = mkSelector "stationary"

-- | @Selector@ for @insufficientlyInUse@
insufficientlyInUseSelector :: Selector
insufficientlyInUseSelector = mkSelector "insufficientlyInUse"

-- | @Selector@ for @locationUnavailable@
locationUnavailableSelector :: Selector
locationUnavailableSelector = mkSelector "locationUnavailable"

-- | @Selector@ for @accuracyLimited@
accuracyLimitedSelector :: Selector
accuracyLimitedSelector = mkSelector "accuracyLimited"

-- | @Selector@ for @serviceSessionRequired@
serviceSessionRequiredSelector :: Selector
serviceSessionRequiredSelector = mkSelector "serviceSessionRequired"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

