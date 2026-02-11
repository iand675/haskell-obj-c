{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMRecordedRotationRateData@.
module ObjC.CoreMotion.CMRecordedRotationRateData
  ( CMRecordedRotationRateData
  , IsCMRecordedRotationRateData(..)
  , startDate
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

-- | @- startDate@
startDate :: IsCMRecordedRotationRateData cmRecordedRotationRateData => cmRecordedRotationRateData -> IO (Id NSDate)
startDate cmRecordedRotationRateData  =
  sendMsg cmRecordedRotationRateData (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

