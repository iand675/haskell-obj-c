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
  , sizeWithWidthDimension_heightDimensionSelector
  , initSelector
  , newSelector
  , widthDimensionSelector
  , heightDimensionSelector


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

-- | @+ sizeWithWidthDimension:heightDimension:@
sizeWithWidthDimension_heightDimension :: (IsNSCollectionLayoutDimension width, IsNSCollectionLayoutDimension height) => width -> height -> IO (Id NSCollectionLayoutSize)
sizeWithWidthDimension_heightDimension width height =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSize"
    withObjCPtr width $ \raw_width ->
      withObjCPtr height $ \raw_height ->
        sendClassMsg cls' (mkSelector "sizeWithWidthDimension:heightDimension:") (retPtr retVoid) [argPtr (castPtr raw_width :: Ptr ()), argPtr (castPtr raw_height :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutSize nsCollectionLayoutSize => nsCollectionLayoutSize -> IO (Id NSCollectionLayoutSize)
init_ nsCollectionLayoutSize  =
  sendMsg nsCollectionLayoutSize (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutSize)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSize"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- widthDimension@
widthDimension :: IsNSCollectionLayoutSize nsCollectionLayoutSize => nsCollectionLayoutSize -> IO (Id NSCollectionLayoutDimension)
widthDimension nsCollectionLayoutSize  =
  sendMsg nsCollectionLayoutSize (mkSelector "widthDimension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- heightDimension@
heightDimension :: IsNSCollectionLayoutSize nsCollectionLayoutSize => nsCollectionLayoutSize -> IO (Id NSCollectionLayoutDimension)
heightDimension nsCollectionLayoutSize  =
  sendMsg nsCollectionLayoutSize (mkSelector "heightDimension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sizeWithWidthDimension:heightDimension:@
sizeWithWidthDimension_heightDimensionSelector :: Selector
sizeWithWidthDimension_heightDimensionSelector = mkSelector "sizeWithWidthDimension:heightDimension:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @widthDimension@
widthDimensionSelector :: Selector
widthDimensionSelector = mkSelector "widthDimension"

-- | @Selector@ for @heightDimension@
heightDimensionSelector :: Selector
heightDimensionSelector = mkSelector "heightDimension"

