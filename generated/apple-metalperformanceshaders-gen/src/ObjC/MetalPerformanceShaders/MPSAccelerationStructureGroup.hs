{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A group of acceleration structures which may be used together in an instance acceleration structure.
--
-- All acceleration structures in an instance acceleration structures must be created with the same group, although they do not all need to be used in the same instance acceleration structure. The acceleration structures in a group share internal GPU memory allocations, so the total number and size of acceleration structures that can be created with the same group is limited by the Metal device's buffer size limits. Therefore, do not group acceleration structures unless they are likely to be used in the same instance acceleration structure.
--
-- Generated bindings for @MPSAccelerationStructureGroup@.
module ObjC.MetalPerformanceShaders.MPSAccelerationStructureGroup
  ( MPSAccelerationStructureGroup
  , IsMPSAccelerationStructureGroup(..)
  , init_
  , initWithDevice
  , device
  , initSelector
  , initWithDeviceSelector
  , deviceSelector


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

-- | @- init@
init_ :: IsMPSAccelerationStructureGroup mpsAccelerationStructureGroup => mpsAccelerationStructureGroup -> IO (Id MPSAccelerationStructureGroup)
init_ mpsAccelerationStructureGroup  =
    sendMsg mpsAccelerationStructureGroup (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSAccelerationStructureGroup mpsAccelerationStructureGroup => mpsAccelerationStructureGroup -> RawId -> IO (Id MPSAccelerationStructureGroup)
initWithDevice mpsAccelerationStructureGroup  device =
    sendMsg mpsAccelerationStructureGroup (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | The Metal device this acceleration structure group was created with
--
-- ObjC selector: @- device@
device :: IsMPSAccelerationStructureGroup mpsAccelerationStructureGroup => mpsAccelerationStructureGroup -> IO RawId
device mpsAccelerationStructureGroup  =
    fmap (RawId . castPtr) $ sendMsg mpsAccelerationStructureGroup (mkSelector "device") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

