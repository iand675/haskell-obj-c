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

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageReduceUnary mpsImageReduceUnary => mpsImageReduceUnary -> RawId -> IO (Id MPSImageReduceUnary)
initWithDevice mpsImageReduceUnary  device =
  sendMsg mpsImageReduceUnary (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

