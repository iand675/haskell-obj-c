{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFaceMetrics@.
module ObjC.SensorKit.SRFaceMetrics
  ( SRFaceMetrics
  , IsSRFaceMetrics(..)
  , init_
  , new
  , version
  , sessionIdentifier
  , context
  , wholeFaceExpressions
  , partialFaceExpressions
  , contextSelector
  , initSelector
  , newSelector
  , partialFaceExpressionsSelector
  , sessionIdentifierSelector
  , versionSelector
  , wholeFaceExpressionsSelector

  -- * Enum types
  , SRFaceMetricsContext(SRFaceMetricsContext)
  , pattern SRFaceMetricsContextDeviceUnlock
  , pattern SRFaceMetricsContextMessagingAppUsage

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id SRFaceMetrics)
init_ srFaceMetrics =
  sendOwnedMessage srFaceMetrics initSelector

-- | @+ new@
new :: IO (Id SRFaceMetrics)
new  =
  do
    cls' <- getRequiredClass "SRFaceMetrics"
    sendOwnedClassMessage cls' newSelector

-- | version
--
-- Algorithm version
--
-- ObjC selector: @- version@
version :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSString)
version srFaceMetrics =
  sendMessage srFaceMetrics versionSelector

-- | sessionIdentifier
--
-- Identifier of a camera session
--
-- ObjC selector: @- sessionIdentifier@
sessionIdentifier :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSString)
sessionIdentifier srFaceMetrics =
  sendMessage srFaceMetrics sessionIdentifierSelector

-- | context
--
-- Indicates system context during a camera session, e.g., if the device was unlocked or (and) a messaging app was used
--
-- ObjC selector: @- context@
context :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO SRFaceMetricsContext
context srFaceMetrics =
  sendMessage srFaceMetrics contextSelector

-- | wholeFaceExpressions
--
-- Detected whole face expressions
--
-- ObjC selector: @- wholeFaceExpressions@
wholeFaceExpressions :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSArray)
wholeFaceExpressions srFaceMetrics =
  sendMessage srFaceMetrics wholeFaceExpressionsSelector

-- | partialFaceExpressions
--
-- Detected partial face expressions
--
-- ObjC selector: @- partialFaceExpressions@
partialFaceExpressions :: IsSRFaceMetrics srFaceMetrics => srFaceMetrics -> IO (Id NSArray)
partialFaceExpressions srFaceMetrics =
  sendMessage srFaceMetrics partialFaceExpressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRFaceMetrics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRFaceMetrics)
newSelector = mkSelector "new"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @sessionIdentifier@
sessionIdentifierSelector :: Selector '[] (Id NSString)
sessionIdentifierSelector = mkSelector "sessionIdentifier"

-- | @Selector@ for @context@
contextSelector :: Selector '[] SRFaceMetricsContext
contextSelector = mkSelector "context"

-- | @Selector@ for @wholeFaceExpressions@
wholeFaceExpressionsSelector :: Selector '[] (Id NSArray)
wholeFaceExpressionsSelector = mkSelector "wholeFaceExpressions"

-- | @Selector@ for @partialFaceExpressions@
partialFaceExpressionsSelector :: Selector '[] (Id NSArray)
partialFaceExpressionsSelector = mkSelector "partialFaceExpressions"

