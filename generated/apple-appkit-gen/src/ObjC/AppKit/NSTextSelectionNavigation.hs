{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextSelectionNavigation@.
module ObjC.AppKit.NSTextSelectionNavigation
  ( NSTextSelectionNavigation
  , IsNSTextSelectionNavigation(..)
  , initWithDataSource
  , new
  , init_
  , flushLayoutCache
  , destinationSelectionForTextSelection_direction_destination_extending_confined
  , textSelectionForSelectionGranularity_enclosingTextSelection
  , resolvedInsertionLocationForTextSelection_writingDirection
  , deletionRangesForTextSelection_direction_destination_allowsDecomposition
  , textSelectionDataSource
  , allowsNonContiguousRanges
  , setAllowsNonContiguousRanges
  , rotatesCoordinateSystemForLayoutOrientation
  , setRotatesCoordinateSystemForLayoutOrientation
  , allowsNonContiguousRangesSelector
  , deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector
  , destinationSelectionForTextSelection_direction_destination_extending_confinedSelector
  , flushLayoutCacheSelector
  , initSelector
  , initWithDataSourceSelector
  , newSelector
  , resolvedInsertionLocationForTextSelection_writingDirectionSelector
  , rotatesCoordinateSystemForLayoutOrientationSelector
  , setAllowsNonContiguousRangesSelector
  , setRotatesCoordinateSystemForLayoutOrientationSelector
  , textSelectionDataSourceSelector
  , textSelectionForSelectionGranularity_enclosingTextSelectionSelector

  -- * Enum types
  , NSTextSelectionGranularity(NSTextSelectionGranularity)
  , pattern NSTextSelectionGranularityCharacter
  , pattern NSTextSelectionGranularityWord
  , pattern NSTextSelectionGranularityParagraph
  , pattern NSTextSelectionGranularityLine
  , pattern NSTextSelectionGranularitySentence
  , NSTextSelectionNavigationDestination(NSTextSelectionNavigationDestination)
  , pattern NSTextSelectionNavigationDestinationCharacter
  , pattern NSTextSelectionNavigationDestinationWord
  , pattern NSTextSelectionNavigationDestinationLine
  , pattern NSTextSelectionNavigationDestinationSentence
  , pattern NSTextSelectionNavigationDestinationParagraph
  , pattern NSTextSelectionNavigationDestinationContainer
  , pattern NSTextSelectionNavigationDestinationDocument
  , NSTextSelectionNavigationDirection(NSTextSelectionNavigationDirection)
  , pattern NSTextSelectionNavigationDirectionForward
  , pattern NSTextSelectionNavigationDirectionBackward
  , pattern NSTextSelectionNavigationDirectionRight
  , pattern NSTextSelectionNavigationDirectionLeft
  , pattern NSTextSelectionNavigationDirectionUp
  , pattern NSTextSelectionNavigationDirectionDown
  , NSTextSelectionNavigationModifier(NSTextSelectionNavigationModifier)
  , pattern NSTextSelectionNavigationModifierExtend
  , pattern NSTextSelectionNavigationModifierVisual
  , pattern NSTextSelectionNavigationModifierMultiple
  , NSTextSelectionNavigationWritingDirection(NSTextSelectionNavigationWritingDirection)
  , pattern NSTextSelectionNavigationWritingDirectionLeftToRight
  , pattern NSTextSelectionNavigationWritingDirectionRightToLeft

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

-- | @- initWithDataSource:@
initWithDataSource :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> RawId -> IO (Id NSTextSelectionNavigation)
initWithDataSource nsTextSelectionNavigation dataSource =
  sendOwnedMessage nsTextSelectionNavigation initWithDataSourceSelector dataSource

-- | @+ new@
new :: IO (Id NSTextSelectionNavigation)
new  =
  do
    cls' <- getRequiredClass "NSTextSelectionNavigation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO (Id NSTextSelectionNavigation)
init_ nsTextSelectionNavigation =
  sendOwnedMessage nsTextSelectionNavigation initSelector

-- | @- flushLayoutCache@
flushLayoutCache :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO ()
flushLayoutCache nsTextSelectionNavigation =
  sendMessage nsTextSelectionNavigation flushLayoutCacheSelector

-- | @- destinationSelectionForTextSelection:direction:destination:extending:confined:@
destinationSelectionForTextSelection_direction_destination_extending_confined :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> textSelection -> NSTextSelectionNavigationDirection -> NSTextSelectionNavigationDestination -> Bool -> Bool -> IO (Id NSTextSelection)
destinationSelectionForTextSelection_direction_destination_extending_confined nsTextSelectionNavigation textSelection direction destination extending confined =
  sendMessage nsTextSelectionNavigation destinationSelectionForTextSelection_direction_destination_extending_confinedSelector (toNSTextSelection textSelection) direction destination extending confined

-- | @- textSelectionForSelectionGranularity:enclosingTextSelection:@
textSelectionForSelectionGranularity_enclosingTextSelection :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> NSTextSelectionGranularity -> textSelection -> IO (Id NSTextSelection)
textSelectionForSelectionGranularity_enclosingTextSelection nsTextSelectionNavigation selectionGranularity textSelection =
  sendMessage nsTextSelectionNavigation textSelectionForSelectionGranularity_enclosingTextSelectionSelector selectionGranularity (toNSTextSelection textSelection)

-- | @- resolvedInsertionLocationForTextSelection:writingDirection:@
resolvedInsertionLocationForTextSelection_writingDirection :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> textSelection -> NSTextSelectionNavigationWritingDirection -> IO RawId
resolvedInsertionLocationForTextSelection_writingDirection nsTextSelectionNavigation textSelection writingDirection =
  sendMessage nsTextSelectionNavigation resolvedInsertionLocationForTextSelection_writingDirectionSelector (toNSTextSelection textSelection) writingDirection

-- | @- deletionRangesForTextSelection:direction:destination:allowsDecomposition:@
deletionRangesForTextSelection_direction_destination_allowsDecomposition :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> textSelection -> NSTextSelectionNavigationDirection -> NSTextSelectionNavigationDestination -> Bool -> IO (Id NSArray)
deletionRangesForTextSelection_direction_destination_allowsDecomposition nsTextSelectionNavigation textSelection direction destination allowsDecomposition =
  sendMessage nsTextSelectionNavigation deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector (toNSTextSelection textSelection) direction destination allowsDecomposition

-- | @- textSelectionDataSource@
textSelectionDataSource :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO RawId
textSelectionDataSource nsTextSelectionNavigation =
  sendMessage nsTextSelectionNavigation textSelectionDataSourceSelector

-- | @- allowsNonContiguousRanges@
allowsNonContiguousRanges :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO Bool
allowsNonContiguousRanges nsTextSelectionNavigation =
  sendMessage nsTextSelectionNavigation allowsNonContiguousRangesSelector

-- | @- setAllowsNonContiguousRanges:@
setAllowsNonContiguousRanges :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> Bool -> IO ()
setAllowsNonContiguousRanges nsTextSelectionNavigation value =
  sendMessage nsTextSelectionNavigation setAllowsNonContiguousRangesSelector value

-- | @- rotatesCoordinateSystemForLayoutOrientation@
rotatesCoordinateSystemForLayoutOrientation :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO Bool
rotatesCoordinateSystemForLayoutOrientation nsTextSelectionNavigation =
  sendMessage nsTextSelectionNavigation rotatesCoordinateSystemForLayoutOrientationSelector

-- | @- setRotatesCoordinateSystemForLayoutOrientation:@
setRotatesCoordinateSystemForLayoutOrientation :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> Bool -> IO ()
setRotatesCoordinateSystemForLayoutOrientation nsTextSelectionNavigation value =
  sendMessage nsTextSelectionNavigation setRotatesCoordinateSystemForLayoutOrientationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDataSource:@
initWithDataSourceSelector :: Selector '[RawId] (Id NSTextSelectionNavigation)
initWithDataSourceSelector = mkSelector "initWithDataSource:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSTextSelectionNavigation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextSelectionNavigation)
initSelector = mkSelector "init"

-- | @Selector@ for @flushLayoutCache@
flushLayoutCacheSelector :: Selector '[] ()
flushLayoutCacheSelector = mkSelector "flushLayoutCache"

-- | @Selector@ for @destinationSelectionForTextSelection:direction:destination:extending:confined:@
destinationSelectionForTextSelection_direction_destination_extending_confinedSelector :: Selector '[Id NSTextSelection, NSTextSelectionNavigationDirection, NSTextSelectionNavigationDestination, Bool, Bool] (Id NSTextSelection)
destinationSelectionForTextSelection_direction_destination_extending_confinedSelector = mkSelector "destinationSelectionForTextSelection:direction:destination:extending:confined:"

-- | @Selector@ for @textSelectionForSelectionGranularity:enclosingTextSelection:@
textSelectionForSelectionGranularity_enclosingTextSelectionSelector :: Selector '[NSTextSelectionGranularity, Id NSTextSelection] (Id NSTextSelection)
textSelectionForSelectionGranularity_enclosingTextSelectionSelector = mkSelector "textSelectionForSelectionGranularity:enclosingTextSelection:"

-- | @Selector@ for @resolvedInsertionLocationForTextSelection:writingDirection:@
resolvedInsertionLocationForTextSelection_writingDirectionSelector :: Selector '[Id NSTextSelection, NSTextSelectionNavigationWritingDirection] RawId
resolvedInsertionLocationForTextSelection_writingDirectionSelector = mkSelector "resolvedInsertionLocationForTextSelection:writingDirection:"

-- | @Selector@ for @deletionRangesForTextSelection:direction:destination:allowsDecomposition:@
deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector :: Selector '[Id NSTextSelection, NSTextSelectionNavigationDirection, NSTextSelectionNavigationDestination, Bool] (Id NSArray)
deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector = mkSelector "deletionRangesForTextSelection:direction:destination:allowsDecomposition:"

-- | @Selector@ for @textSelectionDataSource@
textSelectionDataSourceSelector :: Selector '[] RawId
textSelectionDataSourceSelector = mkSelector "textSelectionDataSource"

-- | @Selector@ for @allowsNonContiguousRanges@
allowsNonContiguousRangesSelector :: Selector '[] Bool
allowsNonContiguousRangesSelector = mkSelector "allowsNonContiguousRanges"

-- | @Selector@ for @setAllowsNonContiguousRanges:@
setAllowsNonContiguousRangesSelector :: Selector '[Bool] ()
setAllowsNonContiguousRangesSelector = mkSelector "setAllowsNonContiguousRanges:"

-- | @Selector@ for @rotatesCoordinateSystemForLayoutOrientation@
rotatesCoordinateSystemForLayoutOrientationSelector :: Selector '[] Bool
rotatesCoordinateSystemForLayoutOrientationSelector = mkSelector "rotatesCoordinateSystemForLayoutOrientation"

-- | @Selector@ for @setRotatesCoordinateSystemForLayoutOrientation:@
setRotatesCoordinateSystemForLayoutOrientationSelector :: Selector '[Bool] ()
setRotatesCoordinateSystemForLayoutOrientationSelector = mkSelector "setRotatesCoordinateSystemForLayoutOrientation:"

