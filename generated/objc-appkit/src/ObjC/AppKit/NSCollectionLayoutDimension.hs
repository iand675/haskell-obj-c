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
  , fractionalWidthDimensionSelector
  , fractionalHeightDimensionSelector
  , absoluteDimensionSelector
  , estimatedDimensionSelector
  , initSelector
  , newSelector
  , isFractionalWidthSelector
  , isFractionalHeightSelector
  , isAbsoluteSelector
  , isEstimatedSelector
  , dimensionSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fractionalWidthDimension:@
fractionalWidthDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
fractionalWidthDimension fractionalWidth =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMsg cls' (mkSelector "fractionalWidthDimension:") (retPtr retVoid) [argCDouble (fromIntegral fractionalWidth)] >>= retainedObject . castPtr

-- | @+ fractionalHeightDimension:@
fractionalHeightDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
fractionalHeightDimension fractionalHeight =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMsg cls' (mkSelector "fractionalHeightDimension:") (retPtr retVoid) [argCDouble (fromIntegral fractionalHeight)] >>= retainedObject . castPtr

-- | @+ absoluteDimension:@
absoluteDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
absoluteDimension absoluteDimension =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMsg cls' (mkSelector "absoluteDimension:") (retPtr retVoid) [argCDouble (fromIntegral absoluteDimension)] >>= retainedObject . castPtr

-- | @+ estimatedDimension:@
estimatedDimension :: CDouble -> IO (Id NSCollectionLayoutDimension)
estimatedDimension estimatedDimension =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMsg cls' (mkSelector "estimatedDimension:") (retPtr retVoid) [argCDouble (fromIntegral estimatedDimension)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO (Id NSCollectionLayoutDimension)
init_ nsCollectionLayoutDimension  =
  sendMsg nsCollectionLayoutDimension (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutDimension)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDimension"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- isFractionalWidth@
isFractionalWidth :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isFractionalWidth nsCollectionLayoutDimension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutDimension (mkSelector "isFractionalWidth") retCULong []

-- | @- isFractionalHeight@
isFractionalHeight :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isFractionalHeight nsCollectionLayoutDimension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutDimension (mkSelector "isFractionalHeight") retCULong []

-- | @- isAbsolute@
isAbsolute :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isAbsolute nsCollectionLayoutDimension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutDimension (mkSelector "isAbsolute") retCULong []

-- | @- isEstimated@
isEstimated :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO Bool
isEstimated nsCollectionLayoutDimension  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutDimension (mkSelector "isEstimated") retCULong []

-- | @- dimension@
dimension :: IsNSCollectionLayoutDimension nsCollectionLayoutDimension => nsCollectionLayoutDimension -> IO CDouble
dimension nsCollectionLayoutDimension  =
  sendMsg nsCollectionLayoutDimension (mkSelector "dimension") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fractionalWidthDimension:@
fractionalWidthDimensionSelector :: Selector
fractionalWidthDimensionSelector = mkSelector "fractionalWidthDimension:"

-- | @Selector@ for @fractionalHeightDimension:@
fractionalHeightDimensionSelector :: Selector
fractionalHeightDimensionSelector = mkSelector "fractionalHeightDimension:"

-- | @Selector@ for @absoluteDimension:@
absoluteDimensionSelector :: Selector
absoluteDimensionSelector = mkSelector "absoluteDimension:"

-- | @Selector@ for @estimatedDimension:@
estimatedDimensionSelector :: Selector
estimatedDimensionSelector = mkSelector "estimatedDimension:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @isFractionalWidth@
isFractionalWidthSelector :: Selector
isFractionalWidthSelector = mkSelector "isFractionalWidth"

-- | @Selector@ for @isFractionalHeight@
isFractionalHeightSelector :: Selector
isFractionalHeightSelector = mkSelector "isFractionalHeight"

-- | @Selector@ for @isAbsolute@
isAbsoluteSelector :: Selector
isAbsoluteSelector = mkSelector "isAbsolute"

-- | @Selector@ for @isEstimated@
isEstimatedSelector :: Selector
isEstimatedSelector = mkSelector "isEstimated"

-- | @Selector@ for @dimension@
dimensionSelector :: Selector
dimensionSelector = mkSelector "dimension"

