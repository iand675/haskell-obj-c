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
import ObjC.Foundation.Internal.Classes

-- | @- identifier@
identifier :: IsCMRecordedPressureData cmRecordedPressureData => cmRecordedPressureData -> IO CULong
identifier cmRecordedPressureData  =
  sendMsg cmRecordedPressureData (mkSelector "identifier") retCULong []

-- | @- startDate@
startDate :: IsCMRecordedPressureData cmRecordedPressureData => cmRecordedPressureData -> IO (Id NSDate)
startDate cmRecordedPressureData  =
  sendMsg cmRecordedPressureData (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

