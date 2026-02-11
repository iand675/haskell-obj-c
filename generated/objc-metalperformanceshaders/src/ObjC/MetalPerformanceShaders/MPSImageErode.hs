{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageErode
--
-- The MPSImageErode filter finds the minimum pixel value in a rectangular region centered around each pixel in the              source image. It is like the MPSImageAreaMin, except that the intensity at each position is calculated relative              to a different value before determining which is the maximum pixel value, allowing for shaped, non-rectangular              morphological probes.
--
-- for each pixel in the filter window:
-- value =  pixel[filterY][filterX] + filter[filterY*filter_width+filterX]
-- if( value < bestValue ){
-- result = value
-- bestValue = value;
-- }
--
-- A filter that contains all zeros is identical to a MPSImageAreaMin filter. The center filter element              is assumed to be 0, to avoid causing a general lightening of the image.
--
-- The definition of the filter for MPSImageErode is different from vImage. (MPSErode_filter_value = 1.0f-vImageErode_filter_value.)              This allows MPSImageDilate and MPSImageErode to use the same filter, making open and close operators easier to write.              The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
--
-- Generated bindings for @MPSImageErode@.
module ObjC.MetalPerformanceShaders.MPSImageErode
  ( MPSImageErode
  , IsMPSImageErode(..)


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

