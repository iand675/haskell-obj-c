{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageAreaMin
--
-- The MPSImageAreaMin finds the minimum pixel value in a rectangular region centered around each pixel in the               source image. If there are multiple channels in the source image, each channel is processed independently.               It has the same methods as MPSImageAreaMax               The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
--
-- Generated bindings for @MPSImageAreaMin@.
module ObjC.MetalPerformanceShaders.MPSImageAreaMin
  ( MPSImageAreaMin
  , IsMPSImageAreaMin(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

