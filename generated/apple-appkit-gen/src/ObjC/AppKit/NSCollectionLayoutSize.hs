{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutSize@.
module ObjC.AppKit.NSCollectionLayoutSize
  ( NSCollectionLayoutSize
  , IsNSCollectionLayoutSize(..)
  , sizeWithWidthDimension_heightDimension
  , init_
  , new
  , widthDimension
  , heightDimension
  , heightDimensionSelector
  , initSelector
  , newSelector
  , sizeWithWidthDimension_heightDimensionSelector
  , widthDimensionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sizeWithWidthDimension:heightDimension:@
sizeWithWidthDimension_heightDimension :: (IsNSCollectionLayoutDimension width, IsNSCollectionLayoutDimension height) => width -> height -> IO (Id NSCollectionLayoutSize)
sizeWithWidthDimension_heightDimension width height =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSize"
    sendClassMessage cls' sizeWithWidthDimension_heightDimensionSelector (toNSCollectionLayoutDimension width) (toNSCollectionLayoutDimension height)

-- | @- init@
init_ :: IsNSCollectionLayoutSize nsCollectionLayoutSize => nsCollectionLayoutSize -> IO (Id NSCollectionLayoutSize)
init_ nsCollectionLayoutSize =
  sendOwnedMessage nsCollectionLayoutSize initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutSize)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSize"
    sendOwnedClassMessage cls' newSelector

-- | @- widthDimension@
widthDimension :: IsNSCollectionLayoutSize nsCollectionLayoutSize => nsCollectionLayoutSize -> IO (Id NSCollectionLayoutDimension)
widthDimension nsCollectionLayoutSize =
  sendMessage nsCollectionLayoutSize widthDimensionSelector

-- | @- heightDimension@
heightDimension :: IsNSCollectionLayoutSize nsCollectionLayoutSize => nsCollectionLayoutSize -> IO (Id NSCollectionLayoutDimension)
heightDimension nsCollectionLayoutSize =
  sendMessage nsCollectionLayoutSize heightDimensionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sizeWithWidthDimension:heightDimension:@
sizeWithWidthDimension_heightDimensionSelector :: Selector '[Id NSCollectionLayoutDimension, Id NSCollectionLayoutDimension] (Id NSCollectionLayoutSize)
sizeWithWidthDimension_heightDimensionSelector = mkSelector "sizeWithWidthDimension:heightDimension:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutSize)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutSize)
newSelector = mkSelector "new"

-- | @Selector@ for @widthDimension@
widthDimensionSelector :: Selector '[] (Id NSCollectionLayoutDimension)
widthDimensionSelector = mkSelector "widthDimension"

-- | @Selector@ for @heightDimension@
heightDimensionSelector :: Selector '[] (Id NSCollectionLayoutDimension)
heightDimensionSelector = mkSelector "heightDimension"

