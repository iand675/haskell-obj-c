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
  , rangeWithLowerBound_upperBoundSelector
  , initWithLowerBound_upperBoundSelector
  , initSelector
  , newSelector
  , lowerBoundSelector
  , upperBoundSelector


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
    sendClassMsg cls' (mkSelector "rangeWithLowerBound:upperBound:") (retPtr retVoid) [argCDouble (fromIntegral lowerBound), argCDouble (fromIntegral upperBound)] >>= retainedObject . castPtr

-- | Creates a range with the bounds you specify.
--
-- - Parameters:   - lowerBound: The lower bound of the range.   - upperBound: The upper bound of the range.
--
-- ObjC selector: @- initWithLowerBound:upperBound:@
initWithLowerBound_upperBound :: IsSHRange shRange => shRange -> CDouble -> CDouble -> IO (Id SHRange)
initWithLowerBound_upperBound shRange  lowerBound upperBound =
  sendMsg shRange (mkSelector "initWithLowerBound:upperBound:") (retPtr retVoid) [argCDouble (fromIntegral lowerBound), argCDouble (fromIntegral upperBound)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSHRange shRange => shRange -> IO (Id SHRange)
init_ shRange  =
  sendMsg shRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SHRange)
new  =
  do
    cls' <- getRequiredClass "SHRange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The lowerBound of this time range
--
-- ObjC selector: @- lowerBound@
lowerBound :: IsSHRange shRange => shRange -> IO CDouble
lowerBound shRange  =
  sendMsg shRange (mkSelector "lowerBound") retCDouble []

-- | The range's upper bound.
--
-- ObjC selector: @- upperBound@
upperBound :: IsSHRange shRange => shRange -> IO CDouble
upperBound shRange  =
  sendMsg shRange (mkSelector "upperBound") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rangeWithLowerBound:upperBound:@
rangeWithLowerBound_upperBoundSelector :: Selector
rangeWithLowerBound_upperBoundSelector = mkSelector "rangeWithLowerBound:upperBound:"

-- | @Selector@ for @initWithLowerBound:upperBound:@
initWithLowerBound_upperBoundSelector :: Selector
initWithLowerBound_upperBoundSelector = mkSelector "initWithLowerBound:upperBound:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector
upperBoundSelector = mkSelector "upperBound"

