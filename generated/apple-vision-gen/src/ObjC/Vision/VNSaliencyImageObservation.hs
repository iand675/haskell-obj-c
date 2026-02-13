{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of bounds of salient objects within the image. Each box represents a distinct mode of the heat map.
--
-- ObjC selector: @- salientObjects@
salientObjects :: IsVNSaliencyImageObservation vnSaliencyImageObservation => vnSaliencyImageObservation -> IO (Id NSArray)
salientObjects vnSaliencyImageObservation =
  sendMessage vnSaliencyImageObservation salientObjectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @salientObjects@
salientObjectsSelector :: Selector '[] (Id NSArray)
salientObjectsSelector = mkSelector "salientObjects"

