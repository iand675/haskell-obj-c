{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDate@.
module ObjC.SensorKit.NSDate
  ( NSDate
  , IsNSDate(..)
  , dateWithSRAbsoluteTime
  , initWithSRAbsoluteTime
  , srAbsoluteTime
  , dateWithSRAbsoluteTimeSelector
  , initWithSRAbsoluteTimeSelector
  , srAbsoluteTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ dateWithSRAbsoluteTime:@
dateWithSRAbsoluteTime :: CDouble -> IO (Id NSDate)
dateWithSRAbsoluteTime time =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMessage cls' dateWithSRAbsoluteTimeSelector time

-- | @- initWithSRAbsoluteTime:@
initWithSRAbsoluteTime :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithSRAbsoluteTime nsDate time =
  sendOwnedMessage nsDate initWithSRAbsoluteTimeSelector time

-- | @- srAbsoluteTime@
srAbsoluteTime :: IsNSDate nsDate => nsDate -> IO CDouble
srAbsoluteTime nsDate =
  sendMessage nsDate srAbsoluteTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dateWithSRAbsoluteTime:@
dateWithSRAbsoluteTimeSelector :: Selector '[CDouble] (Id NSDate)
dateWithSRAbsoluteTimeSelector = mkSelector "dateWithSRAbsoluteTime:"

-- | @Selector@ for @initWithSRAbsoluteTime:@
initWithSRAbsoluteTimeSelector :: Selector '[CDouble] (Id NSDate)
initWithSRAbsoluteTimeSelector = mkSelector "initWithSRAbsoluteTime:"

-- | @Selector@ for @srAbsoluteTime@
srAbsoluteTimeSelector :: Selector '[] CDouble
srAbsoluteTimeSelector = mkSelector "srAbsoluteTime"

