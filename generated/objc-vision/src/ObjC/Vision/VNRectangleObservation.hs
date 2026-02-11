{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRectangleObservation
--
-- VNDetectedObjectObservation
--
-- VNRectangleObservation is the result of a rectangle detector
--
-- The VNRectangleObservation has a bounding box that encompasses the rectangle found in the image. The rectangle itself is defined by the four corner point properties. The rectangle can be rotated in or even out of plane. A common use case is to use the CIPerspectiveTransform filter to correct a detected rectangle to its 'flat upright' representation. All coordinates are normalized and the coordinates can be outside the image.
--
-- Generated bindings for @VNRectangleObservation@.
module ObjC.Vision.VNRectangleObservation
  ( VNRectangleObservation
  , IsVNRectangleObservation(..)


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

