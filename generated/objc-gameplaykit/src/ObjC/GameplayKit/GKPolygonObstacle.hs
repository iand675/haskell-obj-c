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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Number of vertices on this polygon
--
-- ObjC selector: @- vertexCount@
vertexCount :: IsGKPolygonObstacle gkPolygonObstacle => gkPolygonObstacle -> IO CULong
vertexCount gkPolygonObstacle  =
  sendMsg gkPolygonObstacle (mkSelector "vertexCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector
vertexCountSelector = mkSelector "vertexCount"

