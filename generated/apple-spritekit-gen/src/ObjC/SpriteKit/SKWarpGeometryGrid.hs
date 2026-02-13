{-# LANGUAGE DataKinds #-}
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
  , gridSelector
  , gridWithColumns_rowsSelector
  , initWithCoderSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , vertexCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsSKWarpGeometryGrid skWarpGeometryGrid, IsNSCoder aDecoder) => skWarpGeometryGrid -> aDecoder -> IO (Id SKWarpGeometryGrid)
initWithCoder skWarpGeometryGrid aDecoder =
  sendOwnedMessage skWarpGeometryGrid initWithCoderSelector (toNSCoder aDecoder)

-- | @+ grid@
grid :: IO (Id SKWarpGeometryGrid)
grid  =
  do
    cls' <- getRequiredClass "SKWarpGeometryGrid"
    sendClassMessage cls' gridSelector

-- | @+ gridWithColumns:rows:@
gridWithColumns_rows :: CLong -> CLong -> IO (Id SKWarpGeometryGrid)
gridWithColumns_rows cols rows =
  do
    cls' <- getRequiredClass "SKWarpGeometryGrid"
    sendClassMessage cls' gridWithColumns_rowsSelector cols rows

-- | @- numberOfColumns@
numberOfColumns :: IsSKWarpGeometryGrid skWarpGeometryGrid => skWarpGeometryGrid -> IO CLong
numberOfColumns skWarpGeometryGrid =
  sendMessage skWarpGeometryGrid numberOfColumnsSelector

-- | @- numberOfRows@
numberOfRows :: IsSKWarpGeometryGrid skWarpGeometryGrid => skWarpGeometryGrid -> IO CLong
numberOfRows skWarpGeometryGrid =
  sendMessage skWarpGeometryGrid numberOfRowsSelector

-- | @- vertexCount@
vertexCount :: IsSKWarpGeometryGrid skWarpGeometryGrid => skWarpGeometryGrid -> IO CLong
vertexCount skWarpGeometryGrid =
  sendMessage skWarpGeometryGrid vertexCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKWarpGeometryGrid)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @grid@
gridSelector :: Selector '[] (Id SKWarpGeometryGrid)
gridSelector = mkSelector "grid"

-- | @Selector@ for @gridWithColumns:rows:@
gridWithColumns_rowsSelector :: Selector '[CLong, CLong] (Id SKWarpGeometryGrid)
gridWithColumns_rowsSelector = mkSelector "gridWithColumns:rows:"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CLong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CLong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @vertexCount@
vertexCountSelector :: Selector '[] CLong
vertexCountSelector = mkSelector "vertexCount"

