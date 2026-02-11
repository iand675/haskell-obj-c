{-# LANGUAGE PatternSynonyms #-}
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
  , boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector
  , boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector
  , initSelector
  , newSelector
  , extendsBoundarySelector
  , setExtendsBoundarySelector
  , pinToVisibleBoundsSelector
  , setPinToVisibleBoundsSelector
  , alignmentSelector
  , offsetSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr elementKind $ \raw_elementKind ->
        sendClassMsg cls' (mkSelector "boundarySupplementaryItemWithLayoutSize:elementKind:alignment:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_elementKind :: Ptr ()), argCLong (coerce alignment)] >>= retainedObject . castPtr

-- | @+ boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffset :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind) => layoutSize -> elementKind -> NSRectAlignment -> NSPoint -> IO (Id NSCollectionLayoutBoundarySupplementaryItem)
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffset layoutSize elementKind alignment absoluteOffset =
  do
    cls' <- getRequiredClass "NSCollectionLayoutBoundarySupplementaryItem"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr elementKind $ \raw_elementKind ->
        sendClassMsg cls' (mkSelector "boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_elementKind :: Ptr ()), argCLong (coerce alignment), argNSPoint absoluteOffset] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO (Id NSCollectionLayoutBoundarySupplementaryItem)
init_ nsCollectionLayoutBoundarySupplementaryItem  =
  sendMsg nsCollectionLayoutBoundarySupplementaryItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutBoundarySupplementaryItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutBoundarySupplementaryItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- extendsBoundary@
extendsBoundary :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO Bool
extendsBoundary nsCollectionLayoutBoundarySupplementaryItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutBoundarySupplementaryItem (mkSelector "extendsBoundary") retCULong []

-- | @- setExtendsBoundary:@
setExtendsBoundary :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> Bool -> IO ()
setExtendsBoundary nsCollectionLayoutBoundarySupplementaryItem  value =
  sendMsg nsCollectionLayoutBoundarySupplementaryItem (mkSelector "setExtendsBoundary:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pinToVisibleBounds@
pinToVisibleBounds :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO Bool
pinToVisibleBounds nsCollectionLayoutBoundarySupplementaryItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionLayoutBoundarySupplementaryItem (mkSelector "pinToVisibleBounds") retCULong []

-- | @- setPinToVisibleBounds:@
setPinToVisibleBounds :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> Bool -> IO ()
setPinToVisibleBounds nsCollectionLayoutBoundarySupplementaryItem  value =
  sendMsg nsCollectionLayoutBoundarySupplementaryItem (mkSelector "setPinToVisibleBounds:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alignment@
alignment :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO NSRectAlignment
alignment nsCollectionLayoutBoundarySupplementaryItem  =
  fmap (coerce :: CLong -> NSRectAlignment) $ sendMsg nsCollectionLayoutBoundarySupplementaryItem (mkSelector "alignment") retCLong []

-- | @- offset@
offset :: IsNSCollectionLayoutBoundarySupplementaryItem nsCollectionLayoutBoundarySupplementaryItem => nsCollectionLayoutBoundarySupplementaryItem -> IO NSPoint
offset nsCollectionLayoutBoundarySupplementaryItem  =
  sendMsgStret nsCollectionLayoutBoundarySupplementaryItem (mkSelector "offset") retNSPoint []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundarySupplementaryItemWithLayoutSize:elementKind:alignment:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector :: Selector
boundarySupplementaryItemWithLayoutSize_elementKind_alignmentSelector = mkSelector "boundarySupplementaryItemWithLayoutSize:elementKind:alignment:"

-- | @Selector@ for @boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:@
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector :: Selector
boundarySupplementaryItemWithLayoutSize_elementKind_alignment_absoluteOffsetSelector = mkSelector "boundarySupplementaryItemWithLayoutSize:elementKind:alignment:absoluteOffset:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @extendsBoundary@
extendsBoundarySelector :: Selector
extendsBoundarySelector = mkSelector "extendsBoundary"

-- | @Selector@ for @setExtendsBoundary:@
setExtendsBoundarySelector :: Selector
setExtendsBoundarySelector = mkSelector "setExtendsBoundary:"

-- | @Selector@ for @pinToVisibleBounds@
pinToVisibleBoundsSelector :: Selector
pinToVisibleBoundsSelector = mkSelector "pinToVisibleBounds"

-- | @Selector@ for @setPinToVisibleBounds:@
setPinToVisibleBoundsSelector :: Selector
setPinToVisibleBoundsSelector = mkSelector "setPinToVisibleBounds:"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

