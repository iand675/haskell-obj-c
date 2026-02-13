{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMRecordedAccelerometerData@.
module ObjC.CoreMotion.CMRecordedAccelerometerData
  ( CMRecordedAccelerometerData
  , IsCMRecordedAccelerometerData(..)
  , identifier
  , startDate
  , identifierSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsCMRecordedAccelerometerData cmRecordedAccelerometerData => cmRecordedAccelerometerData -> IO CULong
identifier cmRecordedAccelerometerData =
  sendMessage cmRecordedAccelerometerData identifierSelector

-- | @- startDate@
startDate :: IsCMRecordedAccelerometerData cmRecordedAccelerometerData => cmRecordedAccelerometerData -> IO (Id NSDate)
startDate cmRecordedAccelerometerData =
  sendMessage cmRecordedAccelerometerData startDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] CULong
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

