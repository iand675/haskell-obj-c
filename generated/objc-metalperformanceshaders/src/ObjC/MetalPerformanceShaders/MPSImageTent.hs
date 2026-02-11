{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageTent
--
-- The box filter, while fast, may yield square-ish looking blur effects. However, multiple              passes of the box filter tend to smooth out with each additional pass. For example, two 3-wide              box blurs produces the same effective convolution as a 5-wide tent blur:
--
-- 1   1   1
-- 1   1   1
-- +       1   1   1
-- =================
-- 1   2   3   2   1
--
-- Addition passes tend to approximate a gaussian line shape.
--
-- The MPSImageTent convolves an image with a tent filter. These form a tent shape with incrementally              increasing sides, for example:
--
-- 1   2   3   2   1
--
-- 1   2   1                  2   4   2                  1   2   1
--
-- Like the box filter, this arrangement allows for much faster algorithms, espcially for for larger blur              radii but with a more pleasing appearance.
--
-- The tent blur is a separable filter. The implementation is aware of this and will act accordingly              to give best performance for multi-dimensional blurs.
--
-- Generated bindings for @MPSImageTent@.
module ObjC.MetalPerformanceShaders.MPSImageTent
  ( MPSImageTent
  , IsMPSImageTent(..)


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

