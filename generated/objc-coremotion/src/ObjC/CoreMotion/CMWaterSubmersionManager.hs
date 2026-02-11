{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMWaterSubmersionManager@.
module ObjC.CoreMotion.CMWaterSubmersionManager
  ( CMWaterSubmersionManager
  , IsCMWaterSubmersionManager(..)
  , waterSubmersionAvailable
  , authorizationStatus
  , maximumDepth
  , waterSubmersionAvailableSelector
  , authorizationStatusSelector
  , maximumDepthSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

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

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ waterSubmersionAvailable@
waterSubmersionAvailable :: IO Bool
waterSubmersionAvailable  =
  do
    cls' <- getRequiredClass "CMWaterSubmersionManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "waterSubmersionAvailable") retCULong []

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMWaterSubmersionManager"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- maximumDepth@
maximumDepth :: IsCMWaterSubmersionManager cmWaterSubmersionManager => cmWaterSubmersionManager -> IO (Id NSMeasurement)
maximumDepth cmWaterSubmersionManager  =
  sendMsg cmWaterSubmersionManager (mkSelector "maximumDepth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @waterSubmersionAvailable@
waterSubmersionAvailableSelector :: Selector
waterSubmersionAvailableSelector = mkSelector "waterSubmersionAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @maximumDepth@
maximumDepthSelector :: Selector
maximumDepthSelector = mkSelector "maximumDepth"

