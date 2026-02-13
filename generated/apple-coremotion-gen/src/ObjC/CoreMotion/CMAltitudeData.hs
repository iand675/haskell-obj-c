{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMAltitudeData@.
module ObjC.CoreMotion.CMAltitudeData
  ( CMAltitudeData
  , IsCMAltitudeData(..)
  , relativeAltitude
  , pressure
  , pressureSelector
  , relativeAltitudeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- relativeAltitude@
relativeAltitude :: IsCMAltitudeData cmAltitudeData => cmAltitudeData -> IO (Id NSNumber)
relativeAltitude cmAltitudeData =
  sendMessage cmAltitudeData relativeAltitudeSelector

-- | @- pressure@
pressure :: IsCMAltitudeData cmAltitudeData => cmAltitudeData -> IO (Id NSNumber)
pressure cmAltitudeData =
  sendMessage cmAltitudeData pressureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @relativeAltitude@
relativeAltitudeSelector :: Selector '[] (Id NSNumber)
relativeAltitudeSelector = mkSelector "relativeAltitude"

-- | @Selector@ for @pressure@
pressureSelector :: Selector '[] (Id NSNumber)
pressureSelector = mkSelector "pressure"

