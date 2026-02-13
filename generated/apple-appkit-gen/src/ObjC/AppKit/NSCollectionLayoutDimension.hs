{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutDimension@.
module ObjC.AppKit.NSCollectionLayoutDimension
  ( NSCollectionLayoutDimension
  , IsNSCollectionLayoutDimension(..)
  , fractionalWidthDimension
  , fractionalHeightDimension
  , absoluteDimension
  , estimatedDimension
  , init_
  , new
  , isFractionalWidth
  , isFractionalHeight
  , isAbsolute
  , isEstimated
  , dimension
  , absoluteDimensionSelector
  , dimensionSelector
  , estimatedDimensionSelector
  , fractionalHeightDimensionSelector
  , fractionalWidthDimensionSelector
  , initSelector
  , isAbsoluteSelector
  , isEstimatedSelector
  , isFractionalHeightSelector
  , isFractionalWidthSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fractionalWidthDimension:@
fractionalWidthDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
fractionalWidthDimension fractionalWidth =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMessage cls' fractionalWidthDimensionSelector fractionalWidth

-- | @+ fractionalHeightDimension:@
fractionalHeightDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
fractionalHeightDimension fractionalHeight =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMessage cls' fractionalHeightDimensionSelector fractionalHeight

-- | @+ absoluteDimension:@
absoluteDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
absoluteDimension absoluteDimension =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMessage cls' absoluteDimensionSelector absoluteDimension

-- | @+ estimatedDimension:@
estimatedDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
estimatedDimension estimatedDimension =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMessage cls' estimatedDimensionSelector estimatedDimension

-- | @- init@
init_ :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO (Id NSCollectionLayoutDimension)
init_ nsCollectionLayoutDimension =
  sendOwnedMessage nsCollectionLayoutDimension initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutDimension)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendOwnedClassMessage cls' newSelector

-- | @- isFractionalWidth@
isFractionalWidth :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isFractionalWidth nsCollectionLayoutDimension =
  sendMessage nsCollectionLayoutDimension isFractionalWidthSelector

-- | @- isFractionalHeight@
isFractionalHeight :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isFractionalHeight nsCollectionLayoutDimension =
  sendMessage nsCollectionLayoutDimension isFractionalHeightSelector

-- | @- isAbsolute@
isAbsolute :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isAbsolute nsCollectionLayoutDimension =
  sendMessage nsCollectionLayoutDimension isAbsoluteSelector

-- | @- isEstimated@
isEstimated :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isEstimated nsCollectionLayoutDimension =
  sendMessage nsCollectionLayoutDimension isEstimatedSelector

-- | @- dimension@
dimension :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO CDouble
dimension nsCollectionLayoutDimension =
  sendMessage nsCollectionLayoutDimension dimensionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fractionalWidthDimension:@
fractionalWidthDimensionSelector :: Selector '[CDouble] (Id NSCollectionLayoutDimension)
fractionalWidthDimensionSelector = mkSelector "fractionalWidthDimension:"

-- | @Selector@ for @fractionalHeightDimension:@
fractionalHeightDimensionSelector :: Selector '[CDouble] (Id NSCollectionLayoutDimension)
fractionalHeightDimensionSelector = mkSelector "fractionalHeightDimension:"

-- | @Selector@ for @absoluteDimension:@
absoluteDimensionSelector :: Selector '[CDouble] (Id NSCollectionLayoutDimension)
absoluteDimensionSelector = mkSelector "absoluteDimension:"

-- | @Selector@ for @estimatedDimension:@
estimatedDimensionSelector :: Selector '[CDouble] (Id NSCollectionLayoutDimension)
estimatedDimensionSelector = mkSelector "estimatedDimension:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutDimension)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutDimension)
newSelector = mkSelector "new"

-- | @Selector@ for @isFractionalWidth@
isFractionalWidthSelector :: Selector '[] Bool
isFractionalWidthSelector = mkSelector "isFractionalWidth"

-- | @Selector@ for @isFractionalHeight@
isFractionalHeightSelector :: Selector '[] Bool
isFractionalHeightSelector = mkSelector "isFractionalHeight"

-- | @Selector@ for @isAbsolute@
isAbsoluteSelector :: Selector '[] Bool
isAbsoluteSelector = mkSelector "isAbsolute"

-- | @Selector@ for @isEstimated@
isEstimatedSelector :: Selector '[] Bool
isEstimatedSelector = mkSelector "isEstimated"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector '[] CDouble
dimensionSelector = mkSelector "dimension"

