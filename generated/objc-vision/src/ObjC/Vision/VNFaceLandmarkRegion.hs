{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNFaceLandmarkRegion
--
-- VNFaceLandmarkRegion is an immutable object acting as a collection of landmark points for defining a specific region of the face (including potentially all of the landmark points for a face). The VNFaceLandmarkRegion is an abstract base class.
--
-- Generated bindings for @VNFaceLandmarkRegion@.
module ObjC.Vision.VNFaceLandmarkRegion
  ( VNFaceLandmarkRegion
  , IsVNFaceLandmarkRegion(..)
  , new
  , init_
  , pointCount
  , newSelector
  , initSelector
  , pointCountSelector


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

-- | @+ new@
new :: IO (Id VNFaceLandmarkRegion)
new  =
  do
    cls' <- getRequiredClass "VNFaceLandmarkRegion"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNFaceLandmarkRegion vnFaceLandmarkRegion => vnFaceLandmarkRegion -> IO (Id VNFaceLandmarkRegion)
init_ vnFaceLandmarkRegion  =
  sendMsg vnFaceLandmarkRegion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | pointCount returns the amount of points in a given region. This can be zero if no points for a region could be found.
--
-- ObjC selector: @- pointCount@
pointCount :: IsVNFaceLandmarkRegion vnFaceLandmarkRegion => vnFaceLandmarkRegion -> IO CULong
pointCount vnFaceLandmarkRegion  =
  sendMsg vnFaceLandmarkRegion (mkSelector "pointCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @pointCount@
pointCountSelector :: Selector
pointCountSelector = mkSelector "pointCount"

