{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector
  , pointCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNFaceLandmarkRegion)
new  =
  do
    cls' <- getRequiredClass "VNFaceLandmarkRegion"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNFaceLandmarkRegion vnFaceLandmarkRegion => vnFaceLandmarkRegion -> IO (Id VNFaceLandmarkRegion)
init_ vnFaceLandmarkRegion =
  sendOwnedMessage vnFaceLandmarkRegion initSelector

-- | pointCount returns the amount of points in a given region. This can be zero if no points for a region could be found.
--
-- ObjC selector: @- pointCount@
pointCount :: IsVNFaceLandmarkRegion vnFaceLandmarkRegion => vnFaceLandmarkRegion -> IO CULong
pointCount vnFaceLandmarkRegion =
  sendMessage vnFaceLandmarkRegion pointCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNFaceLandmarkRegion)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNFaceLandmarkRegion)
initSelector = mkSelector "init"

-- | @Selector@ for @pointCount@
pointCountSelector :: Selector '[] CULong
pointCountSelector = mkSelector "pointCount"

