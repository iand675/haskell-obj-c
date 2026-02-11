{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNGridSample@.
module ObjC.MetalPerformanceShaders.MPSNNGridSample
  ( MPSNNGridSample
  , IsMPSNNGridSample(..)
  , initWithDevice
  , initWithCoder_device
  , useGridValueAsInputCoordinate
  , setUseGridValueAsInputCoordinate
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , useGridValueAsInputCoordinateSelector
  , setUseGridValueAsInputCoordinateSelector


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

-- | Create a grid sample kernel.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNGridSample object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNGridSample mpsnnGridSample => mpsnnGridSample -> RawId -> IO (Id MPSNNGridSample)
initWithDevice mpsnnGridSample  device =
  sendMsg mpsnnGridSample (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNGridSample mpsnnGridSample, IsNSCoder aDecoder) => mpsnnGridSample -> aDecoder -> RawId -> IO (Id MPSNNGridSample)
initWithCoder_device mpsnnGridSample  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnGridSample (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | useGridValueAsInputCoordinate
--
-- This determines whether the pixel locations from the grid are used as the input coordinate (if set to YES) or              is added to the input coordinate (if set to NO).              The default value is YES.
--
-- ObjC selector: @- useGridValueAsInputCoordinate@
useGridValueAsInputCoordinate :: IsMPSNNGridSample mpsnnGridSample => mpsnnGridSample -> IO Bool
useGridValueAsInputCoordinate mpsnnGridSample  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnGridSample (mkSelector "useGridValueAsInputCoordinate") retCULong []

-- | useGridValueAsInputCoordinate
--
-- This determines whether the pixel locations from the grid are used as the input coordinate (if set to YES) or              is added to the input coordinate (if set to NO).              The default value is YES.
--
-- ObjC selector: @- setUseGridValueAsInputCoordinate:@
setUseGridValueAsInputCoordinate :: IsMPSNNGridSample mpsnnGridSample => mpsnnGridSample -> Bool -> IO ()
setUseGridValueAsInputCoordinate mpsnnGridSample  value =
  sendMsg mpsnnGridSample (mkSelector "setUseGridValueAsInputCoordinate:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @useGridValueAsInputCoordinate@
useGridValueAsInputCoordinateSelector :: Selector
useGridValueAsInputCoordinateSelector = mkSelector "useGridValueAsInputCoordinate"

-- | @Selector@ for @setUseGridValueAsInputCoordinate:@
setUseGridValueAsInputCoordinateSelector :: Selector
setUseGridValueAsInputCoordinateSelector = mkSelector "setUseGridValueAsInputCoordinate:"

