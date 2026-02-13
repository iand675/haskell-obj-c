{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An obstacle with an impassible closed polygon
--
-- Generated bindings for @GKPolygonObstacle@.
module ObjC.GameplayKit.GKPolygonObstacle
  ( GKPolygonObstacle
  , IsGKPolygonObstacle(..)
  , vertexCount
  , vertexCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Number of vertices on this polygon
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsGKPolygonObstacle gkPolygonObstacle => gkPolygonObstacle -> IO CULong
vertexCount gkPolygonObstacle =
  sendMessage gkPolygonObstacle vertexCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector '[] CULong
vertexCountSelector = mkSelector "vertexCount"

