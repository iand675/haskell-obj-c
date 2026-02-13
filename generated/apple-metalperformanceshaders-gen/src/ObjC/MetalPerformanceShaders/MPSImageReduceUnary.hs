{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageReduceUnary
--
-- The MPSImageReduce performs a reduction operation              The reduction operations supported are:                   - Reduce row min                   - Reduce column min                   - Reduce row max                   - Reduce column max                   - Reduce row mean                   - Reduce column mean                   - Reduce row sum                   - Reduce column sum
--
-- Generated bindings for @MPSImageReduceUnary@.
module ObjC.MetalPerformanceShaders.MPSImageReduceUnary
  ( MPSImageReduceUnary
  , IsMPSImageReduceUnary(..)
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
initWithDevice :: IsMPSImageReduceUnary mpsImageReduceUnary => mpsImageReduceUnary -> RawId -> IO (Id MPSImageReduceUnary)
initWithDevice mpsImageReduceUnary device =
  sendOwnedMessage mpsImageReduceUnary initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageReduceUnary)
initWithDeviceSelector = mkSelector "initWithDevice:"

