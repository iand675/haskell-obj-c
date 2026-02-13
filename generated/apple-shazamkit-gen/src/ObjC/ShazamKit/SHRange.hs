{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A half-open interval from a lower bound up to, but not including, an upper bound.
--
-- Generated bindings for @SHRange@.
module ObjC.ShazamKit.SHRange
  ( SHRange
  , IsSHRange(..)
  , rangeWithLowerBound_upperBound
  , initWithLowerBound_upperBound
  , init_
  , new
  , lowerBound
  , upperBound
  , initSelector
  , initWithLowerBound_upperBoundSelector
  , lowerBoundSelector
  , newSelector
  , rangeWithLowerBound_upperBoundSelector
  , upperBoundSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a range with the bounds you specify.
--
-- - Parameters:   - lowerBound: The lower bound of the range.   - upperBound: The upper bound of the range.
--
-- ObjC selector: @+ rangeWithLowerBound:upperBound:@
rangeWithLowerBound_upperBound :: CDouble -> CDouble -> IO (Id SHRange)
rangeWithLowerBound_upperBound lowerBound upperBound =
  do
    cls' <- getRequiredClass "SHRange"
    sendClassMessage cls' rangeWithLowerBound_upperBoundSelector lowerBound upperBound

-- | Creates a range with the bounds you specify.
--
-- - Parameters:   - lowerBound: The lower bound of the range.   - upperBound: The upper bound of the range.
--
-- ObjC selector: @- initWithLowerBound:upperBound:@
initWithLowerBound_upperBound :: IsSHRange shRange => shRange -> CDouble -> CDouble -> IO (Id SHRange)
initWithLowerBound_upperBound shRange lowerBound upperBound =
  sendOwnedMessage shRange initWithLowerBound_upperBoundSelector lowerBound upperBound

-- | @- init@
init_ :: IsSHRange shRange => shRange -> IO (Id SHRange)
init_ shRange =
  sendOwnedMessage shRange initSelector

-- | @+ new@
new :: IO (Id SHRange)
new  =
  do
    cls' <- getRequiredClass "SHRange"
    sendOwnedClassMessage cls' newSelector

-- | The lowerBound of this time range
--
-- ObjC selector: @- lowerBound@
lowerBound :: IsSHRange shRange => shRange -> IO CDouble
lowerBound shRange =
  sendMessage shRange lowerBoundSelector

-- | The range's upper bound.
--
-- ObjC selector: @- upperBound@
upperBound :: IsSHRange shRange => shRange -> IO CDouble
upperBound shRange =
  sendMessage shRange upperBoundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rangeWithLowerBound:upperBound:@
rangeWithLowerBound_upperBoundSelector :: Selector '[CDouble, CDouble] (Id SHRange)
rangeWithLowerBound_upperBoundSelector = mkSelector "rangeWithLowerBound:upperBound:"

-- | @Selector@ for @initWithLowerBound:upperBound:@
initWithLowerBound_upperBoundSelector :: Selector '[CDouble, CDouble] (Id SHRange)
initWithLowerBound_upperBoundSelector = mkSelector "initWithLowerBound:upperBound:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SHRange)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SHRange)
newSelector = mkSelector "new"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector '[] CDouble
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector '[] CDouble
upperBoundSelector = mkSelector "upperBound"

