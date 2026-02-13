{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClusterBridgedDeviceBasic@.
module ObjC.Matter.MTRClusterBridgedDeviceBasic
  ( MTRClusterBridgedDeviceBasic
  , IsMTRClusterBridgedDeviceBasic(..)
  , initWithDevice_endpoint_queue
  , initWithDevice_endpoint_queueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBridgedDeviceBasic mtrClusterBridgedDeviceBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterBridgedDeviceBasic -> device -> CUShort -> queue -> IO (Id MTRClusterBridgedDeviceBasic)
initWithDevice_endpoint_queue mtrClusterBridgedDeviceBasic device endpoint queue =
  sendOwnedMessage mtrClusterBridgedDeviceBasic initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterBridgedDeviceBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

