{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDataSourceSelector
  , newSelector
  , initSelector
  , flushLayoutCacheSelector
  , destinationSelectionForTextSelection_direction_destination_extending_confinedSelector
  , textSelectionForSelectionGranularity_enclosingTextSelectionSelector
  , resolvedInsertionLocationForTextSelection_writingDirectionSelector
  , deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector
  , textSelectionDataSourceSelector
  , allowsNonContiguousRangesSelector
  , setAllowsNonContiguousRangesSelector
  , rotatesCoordinateSystemForLayoutOrientationSelector
  , setRotatesCoordinateSystemForLayoutOrientationSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDataSource:@
initWithDataSource :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> RawId -> IO (Id NSTextSelectionNavigation)
initWithDataSource nsTextSelectionNavigation  dataSource =
    sendMsg nsTextSelectionNavigation (mkSelector "initWithDataSource:") (retPtr retVoid) [argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSTextSelectionNavigation)
new  =
  do
    cls' <- getRequiredClass "NSTextSelectionNavigation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO (Id NSTextSelectionNavigation)
init_ nsTextSelectionNavigation  =
    sendMsg nsTextSelectionNavigation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- flushLayoutCache@
flushLayoutCache :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO ()
flushLayoutCache nsTextSelectionNavigation  =
    sendMsg nsTextSelectionNavigation (mkSelector "flushLayoutCache") retVoid []

-- | @- destinationSelectionForTextSelection:direction:destination:extending:confined:@
destinationSelectionForTextSelection_direction_destination_extending_confined :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> textSelection -> NSTextSelectionNavigationDirection -> NSTextSelectionNavigationDestination -> Bool -> Bool -> IO (Id NSTextSelection)
destinationSelectionForTextSelection_direction_destination_extending_confined nsTextSelectionNavigation  textSelection direction destination extending confined =
  withObjCPtr textSelection $ \raw_textSelection ->
      sendMsg nsTextSelectionNavigation (mkSelector "destinationSelectionForTextSelection:direction:destination:extending:confined:") (retPtr retVoid) [argPtr (castPtr raw_textSelection :: Ptr ()), argCLong (coerce direction), argCLong (coerce destination), argCULong (if extending then 1 else 0), argCULong (if confined then 1 else 0)] >>= retainedObject . castPtr

-- | @- textSelectionForSelectionGranularity:enclosingTextSelection:@
textSelectionForSelectionGranularity_enclosingTextSelection :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> NSTextSelectionGranularity -> textSelection -> IO (Id NSTextSelection)
textSelectionForSelectionGranularity_enclosingTextSelection nsTextSelectionNavigation  selectionGranularity textSelection =
  withObjCPtr textSelection $ \raw_textSelection ->
      sendMsg nsTextSelectionNavigation (mkSelector "textSelectionForSelectionGranularity:enclosingTextSelection:") (retPtr retVoid) [argCLong (coerce selectionGranularity), argPtr (castPtr raw_textSelection :: Ptr ())] >>= retainedObject . castPtr

-- | @- resolvedInsertionLocationForTextSelection:writingDirection:@
resolvedInsertionLocationForTextSelection_writingDirection :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> textSelection -> NSTextSelectionNavigationWritingDirection -> IO RawId
resolvedInsertionLocationForTextSelection_writingDirection nsTextSelectionNavigation  textSelection writingDirection =
  withObjCPtr textSelection $ \raw_textSelection ->
      fmap (RawId . castPtr) $ sendMsg nsTextSelectionNavigation (mkSelector "resolvedInsertionLocationForTextSelection:writingDirection:") (retPtr retVoid) [argPtr (castPtr raw_textSelection :: Ptr ()), argCLong (coerce writingDirection)]

-- | @- deletionRangesForTextSelection:direction:destination:allowsDecomposition:@
deletionRangesForTextSelection_direction_destination_allowsDecomposition :: (IsNSTextSelectionNavigation nsTextSelectionNavigation, IsNSTextSelection textSelection) => nsTextSelectionNavigation -> textSelection -> NSTextSelectionNavigationDirection -> NSTextSelectionNavigationDestination -> Bool -> IO (Id NSArray)
deletionRangesForTextSelection_direction_destination_allowsDecomposition nsTextSelectionNavigation  textSelection direction destination allowsDecomposition =
  withObjCPtr textSelection $ \raw_textSelection ->
      sendMsg nsTextSelectionNavigation (mkSelector "deletionRangesForTextSelection:direction:destination:allowsDecomposition:") (retPtr retVoid) [argPtr (castPtr raw_textSelection :: Ptr ()), argCLong (coerce direction), argCLong (coerce destination), argCULong (if allowsDecomposition then 1 else 0)] >>= retainedObject . castPtr

-- | @- textSelectionDataSource@
textSelectionDataSource :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO RawId
textSelectionDataSource nsTextSelectionNavigation  =
    fmap (RawId . castPtr) $ sendMsg nsTextSelectionNavigation (mkSelector "textSelectionDataSource") (retPtr retVoid) []

-- | @- allowsNonContiguousRanges@
allowsNonContiguousRanges :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO Bool
allowsNonContiguousRanges nsTextSelectionNavigation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextSelectionNavigation (mkSelector "allowsNonContiguousRanges") retCULong []

-- | @- setAllowsNonContiguousRanges:@
setAllowsNonContiguousRanges :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> Bool -> IO ()
setAllowsNonContiguousRanges nsTextSelectionNavigation  value =
    sendMsg nsTextSelectionNavigation (mkSelector "setAllowsNonContiguousRanges:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rotatesCoordinateSystemForLayoutOrientation@
rotatesCoordinateSystemForLayoutOrientation :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> IO Bool
rotatesCoordinateSystemForLayoutOrientation nsTextSelectionNavigation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextSelectionNavigation (mkSelector "rotatesCoordinateSystemForLayoutOrientation") retCULong []

-- | @- setRotatesCoordinateSystemForLayoutOrientation:@
setRotatesCoordinateSystemForLayoutOrientation :: IsNSTextSelectionNavigation nsTextSelectionNavigation => nsTextSelectionNavigation -> Bool -> IO ()
setRotatesCoordinateSystemForLayoutOrientation nsTextSelectionNavigation  value =
    sendMsg nsTextSelectionNavigation (mkSelector "setRotatesCoordinateSystemForLayoutOrientation:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDataSource:@
initWithDataSourceSelector :: Selector
initWithDataSourceSelector = mkSelector "initWithDataSource:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @flushLayoutCache@
flushLayoutCacheSelector :: Selector
flushLayoutCacheSelector = mkSelector "flushLayoutCache"

-- | @Selector@ for @destinationSelectionForTextSelection:direction:destination:extending:confined:@
destinationSelectionForTextSelection_direction_destination_extending_confinedSelector :: Selector
destinationSelectionForTextSelection_direction_destination_extending_confinedSelector = mkSelector "destinationSelectionForTextSelection:direction:destination:extending:confined:"

-- | @Selector@ for @textSelectionForSelectionGranularity:enclosingTextSelection:@
textSelectionForSelectionGranularity_enclosingTextSelectionSelector :: Selector
textSelectionForSelectionGranularity_enclosingTextSelectionSelector = mkSelector "textSelectionForSelectionGranularity:enclosingTextSelection:"

-- | @Selector@ for @resolvedInsertionLocationForTextSelection:writingDirection:@
resolvedInsertionLocationForTextSelection_writingDirectionSelector :: Selector
resolvedInsertionLocationForTextSelection_writingDirectionSelector = mkSelector "resolvedInsertionLocationForTextSelection:writingDirection:"

-- | @Selector@ for @deletionRangesForTextSelection:direction:destination:allowsDecomposition:@
deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector :: Selector
deletionRangesForTextSelection_direction_destination_allowsDecompositionSelector = mkSelector "deletionRangesForTextSelection:direction:destination:allowsDecomposition:"

-- | @Selector@ for @textSelectionDataSource@
textSelectionDataSourceSelector :: Selector
textSelectionDataSourceSelector = mkSelector "textSelectionDataSource"

-- | @Selector@ for @allowsNonContiguousRanges@
allowsNonContiguousRangesSelector :: Selector
allowsNonContiguousRangesSelector = mkSelector "allowsNonContiguousRanges"

-- | @Selector@ for @setAllowsNonContiguousRanges:@
setAllowsNonContiguousRangesSelector :: Selector
setAllowsNonContiguousRangesSelector = mkSelector "setAllowsNonContiguousRanges:"

-- | @Selector@ for @rotatesCoordinateSystemForLayoutOrientation@
rotatesCoordinateSystemForLayoutOrientationSelector :: Selector
rotatesCoordinateSystemForLayoutOrientationSelector = mkSelector "rotatesCoordinateSystemForLayoutOrientation"

-- | @Selector@ for @setRotatesCoordinateSystemForLayoutOrientation:@
setRotatesCoordinateSystemForLayoutOrientationSelector :: Selector
setRotatesCoordinateSystemForLayoutOrientationSelector = mkSelector "setRotatesCoordinateSystemForLayoutOrientation:"

