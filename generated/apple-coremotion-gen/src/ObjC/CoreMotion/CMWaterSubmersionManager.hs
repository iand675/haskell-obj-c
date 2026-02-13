{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMWaterSubmersionManager@.
module ObjC.CoreMotion.CMWaterSubmersionManager
  ( CMWaterSubmersionManager
  , IsCMWaterSubmersionManager(..)
  , delegate
  , setDelegate
  , waterSubmersionAvailable
  , authorizationStatus
  , maximumDepth
  , authorizationStatusSelector
  , delegateSelector
  , maximumDepthSelector
  , setDelegateSelector
  , waterSubmersionAvailableSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- delegate@
delegate :: IsCMWaterSubmersionManager cmWaterSubmersionManager => cmWaterSubmersionManager -> IO RawId
delegate cmWaterSubmersionManager =
  sendMessage cmWaterSubmersionManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCMWaterSubmersionManager cmWaterSubmersionManager => cmWaterSubmersionManager -> RawId -> IO ()
setDelegate cmWaterSubmersionManager value =
  sendMessage cmWaterSubmersionManager setDelegateSelector value

-- | @+ waterSubmersionAvailable@
waterSubmersionAvailable :: IO Bool
waterSubmersionAvailable  =
  do
    cls' <- getRequiredClass "CMWaterSubmersionManager"
    sendClassMessage cls' waterSubmersionAvailableSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMWaterSubmersionManager"
    sendClassMessage cls' authorizationStatusSelector

-- | @- maximumDepth@
maximumDepth :: IsCMWaterSubmersionManager cmWaterSubmersionManager => cmWaterSubmersionManager -> IO (Id NSMeasurement)
maximumDepth cmWaterSubmersionManager =
  sendMessage cmWaterSubmersionManager maximumDepthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @waterSubmersionAvailable@
waterSubmersionAvailableSelector :: Selector '[] Bool
waterSubmersionAvailableSelector = mkSelector "waterSubmersionAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @maximumDepth@
maximumDepthSelector :: Selector '[] (Id NSMeasurement)
maximumDepthSelector = mkSelector "maximumDepth"

