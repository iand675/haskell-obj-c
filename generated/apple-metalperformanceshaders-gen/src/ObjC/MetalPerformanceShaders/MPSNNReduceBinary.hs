{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNReduceBinary
--
-- The MPSNNReduce performs a reduction operation              The reduction operations supported are:                   - Reduce feature channels mean
--
-- Generated bindings for @MPSNNReduceBinary@.
module ObjC.MetalPerformanceShaders.MPSNNReduceBinary
  ( MPSNNReduceBinary
  , IsMPSNNReduceBinary(..)
  , initWithDevice
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNReduceBinary mpsnnReduceBinary => mpsnnReduceBinary -> RawId -> IO (Id MPSNNReduceBinary)
initWithDevice mpsnnReduceBinary device =
  sendOwnedMessage mpsnnReduceBinary initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReduceBinary)
initWithDeviceSelector = mkSelector "initWithDevice:"

