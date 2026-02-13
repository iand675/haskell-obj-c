{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNTextObservation
--
-- VNRectangleObservation
--
-- VNTextObservation Describes a text area detected by the VNRequestNameDetectTextRectangles request.
--
-- Generated bindings for @VNTextObservation@.
module ObjC.Vision.VNTextObservation
  ( VNTextObservation
  , IsVNTextObservation(..)
  , characterBoxes
  , characterBoxesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Array of individual character bounding boxes found within the observation's boundingBox.
--
-- If the associated request indicated that it is interested in character boxes by setting the VNDetectTextRectanglesRequest reportCharacterBoxes property to , this property will be non-nil (but may still be empty, depending on the detection results).
--
-- ObjC selector: @- characterBoxes@
characterBoxes :: IsVNTextObservation vnTextObservation => vnTextObservation -> IO (Id NSArray)
characterBoxes vnTextObservation =
  sendMessage vnTextObservation characterBoxesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @characterBoxes@
characterBoxesSelector :: Selector '[] (Id NSArray)
characterBoxesSelector = mkSelector "characterBoxes"

