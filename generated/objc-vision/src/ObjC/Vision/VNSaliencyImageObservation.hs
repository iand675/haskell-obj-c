{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNSaliencyImageObservation
--
-- VNPixelBufferObservation
--
-- VNSaliencyImageObservation provides a grayscale "heat" map of important areas of an image.
--
-- In the revision1, the "heat" map is a OneComponent32Float pixel format CVPixelBuffer.
--
-- Generated bindings for @VNSaliencyImageObservation@.
module ObjC.Vision.VNSaliencyImageObservation
  ( VNSaliencyImageObservation
  , IsVNSaliencyImageObservation(..)
  , salientObjects
  , salientObjectsSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of bounds of salient objects within the image. Each box represents a distinct mode of the heat map.
--
-- ObjC selector: @- salientObjects@
salientObjects :: IsVNSaliencyImageObservation vnSaliencyImageObservation => vnSaliencyImageObservation -> IO (Id NSArray)
salientObjects vnSaliencyImageObservation  =
  sendMsg vnSaliencyImageObservation (mkSelector "salientObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @salientObjects@
salientObjectsSelector :: Selector
salientObjectsSelector = mkSelector "salientObjects"

