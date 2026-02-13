{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAbsoluteAltitudeData@.
module ObjC.CoreMotion.CMAbsoluteAltitudeData
  ( CMAbsoluteAltitudeData
  , IsCMAbsoluteAltitudeData(..)
  , altitude
  , accuracy
  , precision
  , accuracySelector
  , altitudeSelector
  , precisionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- altitude@
altitude :: IsCMAbsoluteAltitudeData cmAbsoluteAltitudeData => cmAbsoluteAltitudeData -> IO CDouble
altitude cmAbsoluteAltitudeData =
  sendMessage cmAbsoluteAltitudeData altitudeSelector

-- | @- accuracy@
accuracy :: IsCMAbsoluteAltitudeData cmAbsoluteAltitudeData => cmAbsoluteAltitudeData -> IO CDouble
accuracy cmAbsoluteAltitudeData =
  sendMessage cmAbsoluteAltitudeData accuracySelector

-- | @- precision@
precision :: IsCMAbsoluteAltitudeData cmAbsoluteAltitudeData => cmAbsoluteAltitudeData -> IO CDouble
precision cmAbsoluteAltitudeData =
  sendMessage cmAbsoluteAltitudeData precisionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @altitude@
altitudeSelector :: Selector '[] CDouble
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @accuracy@
accuracySelector :: Selector '[] CDouble
accuracySelector = mkSelector "accuracy"

-- | @Selector@ for @precision@
precisionSelector :: Selector '[] CDouble
precisionSelector = mkSelector "precision"

