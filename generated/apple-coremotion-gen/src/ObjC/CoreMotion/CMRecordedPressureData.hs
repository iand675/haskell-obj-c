{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMRecordedPressureData@.
module ObjC.CoreMotion.CMRecordedPressureData
  ( CMRecordedPressureData
  , IsCMRecordedPressureData(..)
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
identifier :: IsCMRecordedPressureData cmRecordedPressureData => cmRecordedPressureData -> IO CULong
identifier cmRecordedPressureData =
  sendMessage cmRecordedPressureData identifierSelector

-- | @- startDate@
startDate :: IsCMRecordedPressureData cmRecordedPressureData => cmRecordedPressureData -> IO (Id NSDate)
startDate cmRecordedPressureData =
  sendMessage cmRecordedPressureData startDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] CULong
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

