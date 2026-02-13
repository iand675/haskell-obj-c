{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutBoundarySupplementaryItem@.
module ObjC.AppKit.NSCollectionLayoutBoundarySupplementaryItem
  ( NSCollectionLayoutBoundarySupplementaryItem
  , IsNSCollectionLayoutBoundarySupplementaryItem(..)
  , boundarySupplementaryItemWithLayoutSize_elementKind_alignment
  , boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffset
  , init_
  , new
  , extendsBoundary
  , setExtendsBoundary
  , pinToVisibleBounds
  , setPinToVisibleBounds
  , alignment
  , offset
  , alignmentSelector
  , boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector
  , boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector
  , extendsBoundarySelector
  , initSelector
  , newSelector
  , offsetSelector
  , pinToVisibleBoundsSelector
  , setExtendsBoundarySelector
  , setPinToVisibleBoundsSelector

  -- * Enum types
  , NSRectAlignment(NSRectAlignment)
  , pattern NSRectAlignmentNone
  , pattern NSRectAlignmentTop
  , pattern NSRectAlignmentTopLeading
  , pattern NSRectAlignmentLeading
  , pattern NSRectAlignmentBottomLeading
  , pattern NSRectAlignmentBottom
  , pattern NSRectAlignmentBottomTrailing
  , pattern NSRectAlignmentTrailing
  , pattern NSRectAlignmentTopTrailing

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ boundarySupplementaryItemWithLayoutSize:elementKind:alignment:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignment :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind) => layoutSize -> elementKind -> NSRectAlignment -> IO (Id NSCollectionLayoutBoundarySupplementaryItem)
boundarySupplementaryItemWithLayoutSize_elementKind_alignment layoutSize elementKind alignment =
  do
    cls' <- getRequiredClass "NSCollectionLayoutBoundarySupplementaryItem"
    sendClassMessage cls' boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector (toNSCollectionLayoutSize layoutSize) (toNSString elementKind) alignment

-- | @+ boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffset :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind) => layoutSize -> elementKind -> NSRectAlignment -> NSPoint -> IO (Id NSCollectionLayoutBoundarySupplementaryItem)
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffset layoutSize elementKind alignment absoluteOffset =
  do
    cls' <- getRequiredClass "NSCollectionLayoutBoundarySupplementaryItem"
    sendClassMessage cls' boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector (toNSCollectionLayoutSize layoutSize) (toNSString elementKind) alignment absoluteOffset

-- | @- init@
init_ :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO (Id NSCollectionLayoutBoundarySupplementaryItem)
init_ nsCollectionLayoutBoundarySupplementaryItem =
  sendOwnedMessage nsCollectionLayoutBoundarySupplementaryItem initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutBoundarySupplementaryItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutBoundarySupplementaryItem"
    sendOwnedClassMessage cls' newSelector

-- | @- extendsBoundary@
extendsBoundary :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO Bool
extendsBoundary nsCollectionLayoutBoundarySupplementaryItem =
  sendMessage nsCollectionLayoutBoundarySupplementaryItem extendsBoundarySelector

-- | @- setExtendsBoundary:@
setExtendsBoundary :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> Bool -> IO ()
setExtendsBoundary nsCollectionLayoutBoundarySupplementaryItem value =
  sendMessage nsCollectionLayoutBoundarySupplementaryItem setExtendsBoundarySelector value

-- | @- pinToVisibleBounds@
pinToVisibleBounds :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO Bool
pinToVisibleBounds nsCollectionLayoutBoundarySupplementaryItem =
  sendMessage nsCollectionLayoutBoundarySupplementaryItem pinToVisibleBoundsSelector

-- | @- setPinToVisibleBounds:@
setPinToVisibleBounds :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> Bool -> IO ()
setPinToVisibleBounds nsCollectionLayoutBoundarySupplementaryItem value =
  sendMessage nsCollectionLayoutBoundarySupplementaryItem setPinToVisibleBoundsSelector value

-- | @- alignment@
alignment :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO NSRectAlignment
alignment nsCollectionLayoutBoundarySupplementaryItem =
  sendMessage nsCollectionLayoutBoundarySupplementaryItem alignmentSelector

-- | @- offset@
offset :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO NSPoint
offset nsCollectionLayoutBoundarySupplementaryItem =
  sendMessage nsCollectionLayoutBoundarySupplementaryItem offsetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundarySupplementaryItemWithLayoutSize:elementKind:alignment:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector :: Selector '[Id NSCollectionLayoutSize, Id NSString, NSRectAlignment] (Id NSCollectionLayoutBoundarySupplementaryItem)
boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector = mkSelector "boundarySupplementaryItemWithLayoutSize:elementKind:alignment:"

-- | @Selector@ for @boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector :: Selector '[Id NSCollectionLayoutSize, Id NSString, NSRectAlignment, NSPoint] (Id NSCollectionLayoutBoundarySupplementaryItem)
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector = mkSelector "boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutBoundarySupplementaryItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutBoundarySupplementaryItem)
newSelector = mkSelector "new"

-- | @Selector@ for @extendsBoundary@
extendsBoundarySelector :: Selector '[] Bool
extendsBoundarySelector = mkSelector "extendsBoundary"

-- | @Selector@ for @setExtendsBoundary:@
setExtendsBoundarySelector :: Selector '[Bool] ()
setExtendsBoundarySelector = mkSelector "setExtendsBoundary:"

-- | @Selector@ for @pinToVisibleBounds@
pinToVisibleBoundsSelector :: Selector '[] Bool
pinToVisibleBoundsSelector = mkSelector "pinToVisibleBounds"

-- | @Selector@ for @setPinToVisibleBounds:@
setPinToVisibleBoundsSelector :: Selector '[Bool] ()
setPinToVisibleBoundsSelector = mkSelector "setPinToVisibleBounds:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSRectAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] NSPoint
offsetSelector = mkSelector "offset"

