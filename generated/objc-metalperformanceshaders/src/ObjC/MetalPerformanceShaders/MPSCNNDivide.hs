{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDivide
--
-- This depends on Metal.framework.
--
-- Specifies the division operator.              For each pixel in the primary source image (x) and each pixel in a secondary source image (y),              it applies the following function: result = ((primaryScale * x) / (secondaryScale * y)) + bias.
--
-- Generated bindings for @MPSCNNDivide@.
module ObjC.MetalPerformanceShaders.MPSCNNDivide
  ( MPSCNNDivide
  , IsMPSCNNDivide(..)
  , initWithDevice
  , initWithDeviceSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the division operator
--
-- @device@ — The device the filter will run on.
--
-- Returns: A valid MPSCNNDivide object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNDivide mpscnnDivide => mpscnnDivide -> RawId -> IO (Id MPSCNNDivide)
initWithDevice mpscnnDivide  device =
  sendMsg mpscnnDivide (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

