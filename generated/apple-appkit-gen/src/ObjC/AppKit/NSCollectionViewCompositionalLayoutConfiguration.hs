{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewCompositionalLayoutConfiguration@.
module ObjC.AppKit.NSCollectionViewCompositionalLayoutConfiguration
  ( NSCollectionViewCompositionalLayoutConfiguration
  , IsNSCollectionViewCompositionalLayoutConfiguration(..)
  , scrollDirection
  , setScrollDirection
  , interSectionSpacing
  , setInterSectionSpacing
  , boundarySupplementaryItems
  , setBoundarySupplementaryItems
  , boundarySupplementaryItemsSelector
  , interSectionSpacingSelector
  , scrollDirectionSelector
  , setBoundarySupplementaryItemsSelector
  , setInterSectionSpacingSelector
  , setScrollDirectionSelector

  -- * Enum types
  , NSCollectionViewScrollDirection(NSCollectionViewScrollDirection)
  , pattern NSCollectionViewScrollDirectionVertical
  , pattern NSCollectionViewScrollDirectionHorizontal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- scrollDirection@
scrollDirection :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> IO NSCollectionViewScrollDirection
scrollDirection nsCollectionViewCompositionalLayoutConfiguration =
  sendMessage nsCollectionViewCompositionalLayoutConfiguration scrollDirectionSelector

-- | @- setScrollDirection:@
setScrollDirection :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> NSCollectionViewScrollDirection -> IO ()
setScrollDirection nsCollectionViewCompositionalLayoutConfiguration value =
  sendMessage nsCollectionViewCompositionalLayoutConfiguration setScrollDirectionSelector value

-- | @- interSectionSpacing@
interSectionSpacing :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> IO CDouble
interSectionSpacing nsCollectionViewCompositionalLayoutConfiguration =
  sendMessage nsCollectionViewCompositionalLayoutConfiguration interSectionSpacingSelector

-- | @- setInterSectionSpacing:@
setInterSectionSpacing :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> CDouble -> IO ()
setInterSectionSpacing nsCollectionViewCompositionalLayoutConfiguration value =
  sendMessage nsCollectionViewCompositionalLayoutConfiguration setInterSectionSpacingSelector value

-- | @- boundarySupplementaryItems@
boundarySupplementaryItems :: IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration => nsCollectionViewCompositionalLayoutConfiguration -> IO (Id NSArray)
boundarySupplementaryItems nsCollectionViewCompositionalLayoutConfiguration =
  sendMessage nsCollectionViewCompositionalLayoutConfiguration boundarySupplementaryItemsSelector

-- | @- setBoundarySupplementaryItems:@
setBoundarySupplementaryItems :: (IsNSCollectionViewCompositionalLayoutConfiguration nsCollectionViewCompositionalLayoutConfiguration, IsNSArray value) => nsCollectionViewCompositionalLayoutConfiguration -> value -> IO ()
setBoundarySupplementaryItems nsCollectionViewCompositionalLayoutConfiguration value =
  sendMessage nsCollectionViewCompositionalLayoutConfiguration setBoundarySupplementaryItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scrollDirection@
scrollDirectionSelector :: Selector '[] NSCollectionViewScrollDirection
scrollDirectionSelector = mkSelector "scrollDirection"

-- | @Selector@ for @setScrollDirection:@
setScrollDirectionSelector :: Selector '[NSCollectionViewScrollDirection] ()
setScrollDirectionSelector = mkSelector "setScrollDirection:"

-- | @Selector@ for @interSectionSpacing@
interSectionSpacingSelector :: Selector '[] CDouble
interSectionSpacingSelector = mkSelector "interSectionSpacing"

-- | @Selector@ for @setInterSectionSpacing:@
setInterSectionSpacingSelector :: Selector '[CDouble] ()
setInterSectionSpacingSelector = mkSelector "setInterSectionSpacing:"

-- | @Selector@ for @boundarySupplementaryItems@
boundarySupplementaryItemsSelector :: Selector '[] (Id NSArray)
boundarySupplementaryItemsSelector = mkSelector "boundarySupplementaryItems"

-- | @Selector@ for @setBoundarySupplementaryItems:@
setBoundarySupplementaryItemsSelector :: Selector '[Id NSArray] ()
setBoundarySupplementaryItemsSelector = mkSelector "setBoundarySupplementaryItems:"

