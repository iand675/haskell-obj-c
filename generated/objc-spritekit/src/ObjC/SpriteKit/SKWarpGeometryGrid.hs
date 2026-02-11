{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKWarpGeometryGrid@.
module ObjC.SpriteKit.SKWarpGeometryGrid
  ( SKWarpGeometryGrid
  , IsSKWarpGeometryGrid(..)
  , initWithCoder
  , grid
  , gridWithColumns_rows
  , numberOfColumns
  , numberOfRows
  , vertexCount
  , initWithCoderSelector
  , gridSelector
  , gridWithColumns_rowsSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsSKWarpGeometryGrid skWarpGeometryGrid, IsNSCoder aDecoder) => skWarpGeometryGrid -> aDecoder -> IO (Id SKWarpGeometryGrid)
initWithCoder skWarpGeometryGrid  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skWarpGeometryGrid (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ grid@
grid :: IO (Id SKWarpGeometryGrid)
grid  =
  do
    cls' <- getRequiredClass "SKWarpGeometryGrid"
    sendClassMsg cls' (mkSelector "grid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gridWithColumns:rows:@
gridWithColumns_rows :: CLong -> CLong -> IO (Id SKWarpGeometryGrid)
gridWithColumns_rows cols rows =
  do
    cls' <- getRequiredClass "SKWarpGeometryGrid"
    sendClassMsg cls' (mkSelector "gridWithColumns:rows:") (retPtr retVoid) [argCLong (fromIntegral cols), argCLong (fromIntegral rows)] >>= retainedObject . castPtr

-- | @- numberOfColumns@
numberOfColumns :: IsSKWarpGeometryGrid skWarpGeometryGrid => skWarpGeometryGrid -> IO CLong
numberOfColumns skWarpGeometryGrid  =
  sendMsg skWarpGeometryGrid (mkSelector "numberOfColumns") retCLong []

-- | @- numberOfRows@
numberOfRows :: IsSKWarpGeometryGrid skWarpGeometryGrid => skWarpGeometryGrid -> IO CLong
numberOfRows skWarpGeometryGrid  =
  sendMsg skWarpGeometryGrid (mkSelector "numberOfRows") retCLong []

-- | @- vertexCount@
vertexCount :: IsSKWarpGeometryGrid skWarpGeometryGrid => skWarpGeometryGrid -> IO CLong
vertexCount skWarpGeometryGrid  =
  sendMsg skWarpGeometryGrid (mkSelector "vertexCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @grid@
gridSelector :: Selector
gridSelector = mkSelector "grid"

-- | @Selector@ for @gridWithColumns:rows:@
gridWithColumns_rowsSelector :: Selector
gridWithColumns_rowsSelector = mkSelector "gridWithColumns:rows:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector
vertexCountSelector = mkSelector "vertexCount"

