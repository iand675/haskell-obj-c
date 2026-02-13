{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFaceMetricsExpression@.
module ObjC.SensorKit.SRFaceMetricsExpression
  ( SRFaceMetricsExpression
  , IsSRFaceMetricsExpression(..)
  , init_
  , new
  , identifier
  , value
  , identifierSelector
  , initSelector
  , newSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRFaceMetricsExpression srFaceMetricsExpression => srFaceMetricsExpression -> IO (Id SRFaceMetricsExpression)
init_ srFaceMetricsExpression =
  sendOwnedMessage srFaceMetricsExpression initSelector

-- | @+ new@
new :: IO (Id SRFaceMetricsExpression)
new  =
  do
    cls' <- getRequiredClass "SRFaceMetricsExpression"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- An opaque identifier for the face expression
--
-- More information about what this face expression represents can be found in Apple's developer documentation
--
-- ObjC selector: @- identifier@
identifier :: IsSRFaceMetricsExpression srFaceMetricsExpression => srFaceMetricsExpression -> IO (Id NSString)
identifier srFaceMetricsExpression =
  sendMessage srFaceMetricsExpression identifierSelector

-- | value
--
-- double value indicating the current position of the expression
--
-- ObjC selector: @- value@
value :: IsSRFaceMetricsExpression srFaceMetricsExpression => srFaceMetricsExpression -> IO CDouble
value srFaceMetricsExpression =
  sendMessage srFaceMetricsExpression valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRFaceMetricsExpression)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRFaceMetricsExpression)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CDouble
valueSelector = mkSelector "value"

