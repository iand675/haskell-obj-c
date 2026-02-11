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

-- | Array of individual character bounding boxes found within the observation's boundingBox.
--
-- If the associated request indicated that it is interested in character boxes by setting the VNDetectTextRectanglesRequest reportCharacterBoxes property to , this property will be non-nil (but may still be empty, depending on the detection results).
--
-- ObjC selector: @- characterBoxes@
characterBoxes :: IsVNTextObservation vnTextObservation => vnTextObservation -> IO (Id NSArray)
characterBoxes vnTextObservation  =
  sendMsg vnTextObservation (mkSelector "characterBoxes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @characterBoxes@
characterBoxesSelector :: Selector
characterBoxesSelector = mkSelector "characterBoxes"

