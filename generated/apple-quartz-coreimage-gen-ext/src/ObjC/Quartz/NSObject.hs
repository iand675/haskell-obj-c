{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.Quartz.NSObject
  ( NSObject
  , IsNSObject(..)
  , saveOptions_shouldShowUTType
  , imageBrowserSelectionDidChange
  , imageBrowser_cellWasDoubleClickedAtIndex
  , imageBrowser_cellWasRightClickedAtIndex_withEvent
  , imageBrowser_backgroundWasRightClickedWithEvent
  , imageUID
  , imageRepresentationType
  , imageRepresentation
  , imageVersion
  , imageTitle
  , imageSubtitle
  , numberOfItemsInImageBrowser
  , imageBrowser_itemAtIndex
  , imageBrowser_removeItemsAtIndexes
  , imageBrowser_moveItemsAtIndexes_toIndex
  , imageBrowser_writeItemsAtIndexes_toPasteboard
  , numberOfGroupsInImageBrowser
  , imageBrowser_groupAtIndex
  , quartzFilterManager_didAddFilter
  , quartzFilterManager_didRemoveFilter
  , quartzFilterManager_didModifyFilter
  , quartzFilterManager_didSelectFilter
  , compositionPickerView_didSelectComposition
  , compositionPickerViewDidStartAnimating
  , compositionPickerViewWillStopAnimating
  , compositionParameterView_shouldDisplayParameterWithKey_attributes
  , compositionParameterView_didChangeParameterWithKey
  , selectable
  , saveOptions_shouldShowUTTypeSelector
  , imageBrowserSelectionDidChangeSelector
  , imageBrowser_cellWasDoubleClickedAtIndexSelector
  , imageBrowser_cellWasRightClickedAtIndex_withEventSelector
  , imageBrowser_backgroundWasRightClickedWithEventSelector
  , imageUIDSelector
  , imageRepresentationTypeSelector
  , imageRepresentationSelector
  , imageVersionSelector
  , imageTitleSelector
  , imageSubtitleSelector
  , numberOfItemsInImageBrowserSelector
  , imageBrowser_itemAtIndexSelector
  , imageBrowser_removeItemsAtIndexesSelector
  , imageBrowser_moveItemsAtIndexes_toIndexSelector
  , imageBrowser_writeItemsAtIndexes_toPasteboardSelector
  , numberOfGroupsInImageBrowserSelector
  , imageBrowser_groupAtIndexSelector
  , quartzFilterManager_didAddFilterSelector
  , quartzFilterManager_didRemoveFilterSelector
  , quartzFilterManager_didModifyFilterSelector
  , quartzFilterManager_didSelectFilterSelector
  , compositionPickerView_didSelectCompositionSelector
  , compositionPickerViewDidStartAnimatingSelector
  , compositionPickerViewWillStopAnimatingSelector
  , compositionParameterView_shouldDisplayParameterWithKey_attributesSelector
  , compositionParameterView_didChangeParameterWithKeySelector
  , selectableSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- saveOptions:shouldShowUTType:@
saveOptions_shouldShowUTType :: (IsNSObject nsObject, IsIKSaveOptions saveOptions, IsNSString utType) => nsObject -> saveOptions -> utType -> IO Bool
saveOptions_shouldShowUTType nsObject  saveOptions utType =
  withObjCPtr saveOptions $ \raw_saveOptions ->
    withObjCPtr utType $ \raw_utType ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "saveOptions:shouldShowUTType:") retCULong [argPtr (castPtr raw_saveOptions :: Ptr ()), argPtr (castPtr raw_utType :: Ptr ())]

-- | imageBrowserSelectionDidChange:
--
-- Invoked by 'aBrowser' when the selection did change
--
-- ObjC selector: @- imageBrowserSelectionDidChange:@
imageBrowserSelectionDidChange :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> IO ()
imageBrowserSelectionDidChange nsObject  aBrowser =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "imageBrowserSelectionDidChange:") retVoid [argPtr (castPtr raw_aBrowser :: Ptr ())]

-- | imageBrowser:cellWasDoubleClickedAtIndex:
--
-- Invoked by 'aBrowser' when a cell was double clicked.
--
-- @index@ — Index of the cell that was double clicked.
--
-- ObjC selector: @- imageBrowser:cellWasDoubleClickedAtIndex:@
imageBrowser_cellWasDoubleClickedAtIndex :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> CULong -> IO ()
imageBrowser_cellWasDoubleClickedAtIndex nsObject  aBrowser index =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "imageBrowser:cellWasDoubleClickedAtIndex:") retVoid [argPtr (castPtr raw_aBrowser :: Ptr ()), argCULong index]

-- | imageBrowser:cellWasRightClickedAtIndex:withEvent:
--
-- Invoked by 'aBrowser' when a cell was right clicked or left clicked with the Alt key pressed.
--
-- @index@ — Index of the cell that was right clicked.
--
-- ObjC selector: @- imageBrowser:cellWasRightClickedAtIndex:withEvent:@
imageBrowser_cellWasRightClickedAtIndex_withEvent :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> CULong -> RawId -> IO ()
imageBrowser_cellWasRightClickedAtIndex_withEvent nsObject  aBrowser index event =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "imageBrowser:cellWasRightClickedAtIndex:withEvent:") retVoid [argPtr (castPtr raw_aBrowser :: Ptr ()), argCULong index, argPtr (castPtr (unRawId event) :: Ptr ())]

-- | imageBrowser:backgroundWasRightClickedWithEvent:
--
-- Invoked by 'aBrowser' when a the background was right clicked or left clicked with the Alt key pressed.
--
-- ObjC selector: @- imageBrowser:backgroundWasRightClickedWithEvent:@
imageBrowser_backgroundWasRightClickedWithEvent :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> RawId -> IO ()
imageBrowser_backgroundWasRightClickedWithEvent nsObject  aBrowser event =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "imageBrowser:backgroundWasRightClickedWithEvent:") retVoid [argPtr (castPtr raw_aBrowser :: Ptr ()), argPtr (castPtr (unRawId event) :: Ptr ())]

-- | imageUID
--
-- Returns a unique string that identify this data source item (required).
--
-- The image browser uses this identifier to keep the correspondance between its cache and the data source item
--
-- ObjC selector: @- imageUID@
imageUID :: IsNSObject nsObject => nsObject -> IO (Id NSString)
imageUID nsObject  =
    sendMsg nsObject (mkSelector "imageUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | imageRepresentationType
--
-- Returns the representation of the image to display (required).
--
-- Keys for imageRepresentationType are defined below.
--
-- ObjC selector: @- imageRepresentationType@
imageRepresentationType :: IsNSObject nsObject => nsObject -> IO (Id NSString)
imageRepresentationType nsObject  =
    sendMsg nsObject (mkSelector "imageRepresentationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | imageRepresentation
--
-- Returns the image to display (required). Can return nil if the item has no image to display.
--
-- ObjC selector: @- imageRepresentation@
imageRepresentation :: IsNSObject nsObject => nsObject -> IO RawId
imageRepresentation nsObject  =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "imageRepresentation") (retPtr retVoid) []

-- | imageVersion
--
-- Returns a version of this item. The receiver can return a new version to let the image browser knows that it shouldn't use its cache for this item
--
-- ObjC selector: @- imageVersion@
imageVersion :: IsNSObject nsObject => nsObject -> IO CULong
imageVersion nsObject  =
    sendMsg nsObject (mkSelector "imageVersion") retCULong []

-- | imageTitle
--
-- Returns the title to display as a NSString. Use setValue:forKey: with IKImageBrowserCellsTitleAttributesKey on the IKImageBrowserView instance to set text attributes.
--
-- ObjC selector: @- imageTitle@
imageTitle :: IsNSObject nsObject => nsObject -> IO (Id NSString)
imageTitle nsObject  =
    sendMsg nsObject (mkSelector "imageTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | imageSubtitle
--
-- Returns the subtitle to display as a NSString. Use setValue:forKey: with IKImageBrowserCellsSubtitleAttributesKey on the IKImageBrowserView instance to set text attributes.
--
-- ObjC selector: @- imageSubtitle@
imageSubtitle :: IsNSObject nsObject => nsObject -> IO (Id NSString)
imageSubtitle nsObject  =
    sendMsg nsObject (mkSelector "imageSubtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | numberOfItemsInImageBrowser:
--
-- Returns the number of records managed for aBrowser by the data source object (required).
--
-- An instance of IKImageView uses this method to determine how many cells it should create and display.
--
-- ObjC selector: @- numberOfItemsInImageBrowser:@
numberOfItemsInImageBrowser :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> IO CULong
numberOfItemsInImageBrowser nsObject  aBrowser =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "numberOfItemsInImageBrowser:") retCULong [argPtr (castPtr raw_aBrowser :: Ptr ())]

-- | imageBrowser:itemAtIndex:
--
-- Returns an object for the record in aBrowser corresponding to index index (required).
--
-- The returned object must implement the required methods of IKImageBrowserItem.
--
-- ObjC selector: @- imageBrowser:itemAtIndex:@
imageBrowser_itemAtIndex :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> CULong -> IO RawId
imageBrowser_itemAtIndex nsObject  aBrowser index =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "imageBrowser:itemAtIndex:") (retPtr retVoid) [argPtr (castPtr raw_aBrowser :: Ptr ()), argCULong index]

-- | imageBrowser:removeItemsAtIndexes:
--
-- Invoked by the image browser after it has been determined that a remove operation should be applied (optional)
--
-- The data source should update itself (usually by removing this indexes).
--
-- ObjC selector: @- imageBrowser:removeItemsAtIndexes:@
imageBrowser_removeItemsAtIndexes :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser, IsNSIndexSet indexes) => nsObject -> aBrowser -> indexes -> IO ()
imageBrowser_removeItemsAtIndexes nsObject  aBrowser indexes =
  withObjCPtr aBrowser $ \raw_aBrowser ->
    withObjCPtr indexes $ \raw_indexes ->
        sendMsg nsObject (mkSelector "imageBrowser:removeItemsAtIndexes:") retVoid [argPtr (castPtr raw_aBrowser :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ())]

-- | imageBrowser:moveItemsAtIndexes:toIndex:
--
-- Invoked by the image browser after it has been determined that a reordering operation should be applied (optional).
--
-- The data source should update itself (usually by reordering its elements).
--
-- ObjC selector: @- imageBrowser:moveItemsAtIndexes:toIndex:@
imageBrowser_moveItemsAtIndexes_toIndex :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser, IsNSIndexSet indexes) => nsObject -> aBrowser -> indexes -> CULong -> IO Bool
imageBrowser_moveItemsAtIndexes_toIndex nsObject  aBrowser indexes destinationIndex =
  withObjCPtr aBrowser $ \raw_aBrowser ->
    withObjCPtr indexes $ \raw_indexes ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "imageBrowser:moveItemsAtIndexes:toIndex:") retCULong [argPtr (castPtr raw_aBrowser :: Ptr ()), argPtr (castPtr raw_indexes :: Ptr ()), argCULong destinationIndex]

-- | imageBrowser:writeItemsAtIndexes:toPasteboard:
--
-- This method is called after it has been determined that a drag should begin, but before the drag has been started. 'itemIndexes' contains the indexes that will be participating in the drag. Return the number of items effectively written to the pasteboard.
--
-- optional - drag and drop support
--
-- ObjC selector: @- imageBrowser:writeItemsAtIndexes:toPasteboard:@
imageBrowser_writeItemsAtIndexes_toPasteboard :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser, IsNSIndexSet itemIndexes) => nsObject -> aBrowser -> itemIndexes -> RawId -> IO CULong
imageBrowser_writeItemsAtIndexes_toPasteboard nsObject  aBrowser itemIndexes pasteboard =
  withObjCPtr aBrowser $ \raw_aBrowser ->
    withObjCPtr itemIndexes $ \raw_itemIndexes ->
        sendMsg nsObject (mkSelector "imageBrowser:writeItemsAtIndexes:toPasteboard:") retCULong [argPtr (castPtr raw_aBrowser :: Ptr ()), argPtr (castPtr raw_itemIndexes :: Ptr ()), argPtr (castPtr (unRawId pasteboard) :: Ptr ())]

-- | numberOfGroupsInImageBrowser:
--
-- Returns the number of groups
--
-- this method is optional
--
-- ObjC selector: @- numberOfGroupsInImageBrowser:@
numberOfGroupsInImageBrowser :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> IO CULong
numberOfGroupsInImageBrowser nsObject  aBrowser =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "numberOfGroupsInImageBrowser:") retCULong [argPtr (castPtr raw_aBrowser :: Ptr ())]

-- | imageBrowser:groupAtIndex:
--
-- Returns the group at index 'index'
--
-- A group is defined by a dictionay. Keys for this dictionary are defined below.
--
-- ObjC selector: @- imageBrowser:groupAtIndex:@
imageBrowser_groupAtIndex :: (IsNSObject nsObject, IsIKImageBrowserView aBrowser) => nsObject -> aBrowser -> CULong -> IO (Id NSDictionary)
imageBrowser_groupAtIndex nsObject  aBrowser index =
  withObjCPtr aBrowser $ \raw_aBrowser ->
      sendMsg nsObject (mkSelector "imageBrowser:groupAtIndex:") (retPtr retVoid) [argPtr (castPtr raw_aBrowser :: Ptr ()), argCULong index] >>= retainedObject . castPtr

-- | @- quartzFilterManager:didAddFilter:@
quartzFilterManager_didAddFilter :: (IsNSObject nsObject, IsQuartzFilterManager sender, IsQuartzFilter filter_) => nsObject -> sender -> filter_ -> IO ()
quartzFilterManager_didAddFilter nsObject  sender filter_ =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr filter_ $ \raw_filter_ ->
        sendMsg nsObject (mkSelector "quartzFilterManager:didAddFilter:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_filter_ :: Ptr ())]

-- | @- quartzFilterManager:didRemoveFilter:@
quartzFilterManager_didRemoveFilter :: (IsNSObject nsObject, IsQuartzFilterManager sender, IsQuartzFilter filter_) => nsObject -> sender -> filter_ -> IO ()
quartzFilterManager_didRemoveFilter nsObject  sender filter_ =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr filter_ $ \raw_filter_ ->
        sendMsg nsObject (mkSelector "quartzFilterManager:didRemoveFilter:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_filter_ :: Ptr ())]

-- | @- quartzFilterManager:didModifyFilter:@
quartzFilterManager_didModifyFilter :: (IsNSObject nsObject, IsQuartzFilterManager sender, IsQuartzFilter filter_) => nsObject -> sender -> filter_ -> IO ()
quartzFilterManager_didModifyFilter nsObject  sender filter_ =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr filter_ $ \raw_filter_ ->
        sendMsg nsObject (mkSelector "quartzFilterManager:didModifyFilter:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_filter_ :: Ptr ())]

-- | @- quartzFilterManager:didSelectFilter:@
quartzFilterManager_didSelectFilter :: (IsNSObject nsObject, IsQuartzFilterManager sender, IsQuartzFilter filter_) => nsObject -> sender -> filter_ -> IO ()
quartzFilterManager_didSelectFilter nsObject  sender filter_ =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr filter_ $ \raw_filter_ ->
        sendMsg nsObject (mkSelector "quartzFilterManager:didSelectFilter:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_filter_ :: Ptr ())]

-- | @- compositionPickerView:didSelectComposition:@
compositionPickerView_didSelectComposition :: (IsNSObject nsObject, IsQCCompositionPickerView pickerView, IsQCComposition composition) => nsObject -> pickerView -> composition -> IO ()
compositionPickerView_didSelectComposition nsObject  pickerView composition =
  withObjCPtr pickerView $ \raw_pickerView ->
    withObjCPtr composition $ \raw_composition ->
        sendMsg nsObject (mkSelector "compositionPickerView:didSelectComposition:") retVoid [argPtr (castPtr raw_pickerView :: Ptr ()), argPtr (castPtr raw_composition :: Ptr ())]

-- | @- compositionPickerViewDidStartAnimating:@
compositionPickerViewDidStartAnimating :: (IsNSObject nsObject, IsQCCompositionPickerView pickerView) => nsObject -> pickerView -> IO ()
compositionPickerViewDidStartAnimating nsObject  pickerView =
  withObjCPtr pickerView $ \raw_pickerView ->
      sendMsg nsObject (mkSelector "compositionPickerViewDidStartAnimating:") retVoid [argPtr (castPtr raw_pickerView :: Ptr ())]

-- | @- compositionPickerViewWillStopAnimating:@
compositionPickerViewWillStopAnimating :: (IsNSObject nsObject, IsQCCompositionPickerView pickerView) => nsObject -> pickerView -> IO ()
compositionPickerViewWillStopAnimating nsObject  pickerView =
  withObjCPtr pickerView $ \raw_pickerView ->
      sendMsg nsObject (mkSelector "compositionPickerViewWillStopAnimating:") retVoid [argPtr (castPtr raw_pickerView :: Ptr ())]

-- | @- compositionParameterView:shouldDisplayParameterWithKey:attributes:@
compositionParameterView_shouldDisplayParameterWithKey_attributes :: (IsNSObject nsObject, IsQCCompositionParameterView parameterView, IsNSString portKey, IsNSDictionary portAttributes) => nsObject -> parameterView -> portKey -> portAttributes -> IO Bool
compositionParameterView_shouldDisplayParameterWithKey_attributes nsObject  parameterView portKey portAttributes =
  withObjCPtr parameterView $ \raw_parameterView ->
    withObjCPtr portKey $ \raw_portKey ->
      withObjCPtr portAttributes $ \raw_portAttributes ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "compositionParameterView:shouldDisplayParameterWithKey:attributes:") retCULong [argPtr (castPtr raw_parameterView :: Ptr ()), argPtr (castPtr raw_portKey :: Ptr ()), argPtr (castPtr raw_portAttributes :: Ptr ())]

-- | @- compositionParameterView:didChangeParameterWithKey:@
compositionParameterView_didChangeParameterWithKey :: (IsNSObject nsObject, IsQCCompositionParameterView parameterView, IsNSString portKey) => nsObject -> parameterView -> portKey -> IO ()
compositionParameterView_didChangeParameterWithKey nsObject  parameterView portKey =
  withObjCPtr parameterView $ \raw_parameterView ->
    withObjCPtr portKey $ \raw_portKey ->
        sendMsg nsObject (mkSelector "compositionParameterView:didChangeParameterWithKey:") retVoid [argPtr (castPtr raw_parameterView :: Ptr ()), argPtr (castPtr raw_portKey :: Ptr ())]

-- | selectable
--
-- Returns whether this item is selectable.
--
-- The receiver can implement this methods to forbid selection of this item by returning NO.
--
-- ObjC selector: @- selectable@
selectable :: IsNSObject nsObject => nsObject -> IO Bool
selectable nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "selectable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveOptions:shouldShowUTType:@
saveOptions_shouldShowUTTypeSelector :: Selector
saveOptions_shouldShowUTTypeSelector = mkSelector "saveOptions:shouldShowUTType:"

-- | @Selector@ for @imageBrowserSelectionDidChange:@
imageBrowserSelectionDidChangeSelector :: Selector
imageBrowserSelectionDidChangeSelector = mkSelector "imageBrowserSelectionDidChange:"

-- | @Selector@ for @imageBrowser:cellWasDoubleClickedAtIndex:@
imageBrowser_cellWasDoubleClickedAtIndexSelector :: Selector
imageBrowser_cellWasDoubleClickedAtIndexSelector = mkSelector "imageBrowser:cellWasDoubleClickedAtIndex:"

-- | @Selector@ for @imageBrowser:cellWasRightClickedAtIndex:withEvent:@
imageBrowser_cellWasRightClickedAtIndex_withEventSelector :: Selector
imageBrowser_cellWasRightClickedAtIndex_withEventSelector = mkSelector "imageBrowser:cellWasRightClickedAtIndex:withEvent:"

-- | @Selector@ for @imageBrowser:backgroundWasRightClickedWithEvent:@
imageBrowser_backgroundWasRightClickedWithEventSelector :: Selector
imageBrowser_backgroundWasRightClickedWithEventSelector = mkSelector "imageBrowser:backgroundWasRightClickedWithEvent:"

-- | @Selector@ for @imageUID@
imageUIDSelector :: Selector
imageUIDSelector = mkSelector "imageUID"

-- | @Selector@ for @imageRepresentationType@
imageRepresentationTypeSelector :: Selector
imageRepresentationTypeSelector = mkSelector "imageRepresentationType"

-- | @Selector@ for @imageRepresentation@
imageRepresentationSelector :: Selector
imageRepresentationSelector = mkSelector "imageRepresentation"

-- | @Selector@ for @imageVersion@
imageVersionSelector :: Selector
imageVersionSelector = mkSelector "imageVersion"

-- | @Selector@ for @imageTitle@
imageTitleSelector :: Selector
imageTitleSelector = mkSelector "imageTitle"

-- | @Selector@ for @imageSubtitle@
imageSubtitleSelector :: Selector
imageSubtitleSelector = mkSelector "imageSubtitle"

-- | @Selector@ for @numberOfItemsInImageBrowser:@
numberOfItemsInImageBrowserSelector :: Selector
numberOfItemsInImageBrowserSelector = mkSelector "numberOfItemsInImageBrowser:"

-- | @Selector@ for @imageBrowser:itemAtIndex:@
imageBrowser_itemAtIndexSelector :: Selector
imageBrowser_itemAtIndexSelector = mkSelector "imageBrowser:itemAtIndex:"

-- | @Selector@ for @imageBrowser:removeItemsAtIndexes:@
imageBrowser_removeItemsAtIndexesSelector :: Selector
imageBrowser_removeItemsAtIndexesSelector = mkSelector "imageBrowser:removeItemsAtIndexes:"

-- | @Selector@ for @imageBrowser:moveItemsAtIndexes:toIndex:@
imageBrowser_moveItemsAtIndexes_toIndexSelector :: Selector
imageBrowser_moveItemsAtIndexes_toIndexSelector = mkSelector "imageBrowser:moveItemsAtIndexes:toIndex:"

-- | @Selector@ for @imageBrowser:writeItemsAtIndexes:toPasteboard:@
imageBrowser_writeItemsAtIndexes_toPasteboardSelector :: Selector
imageBrowser_writeItemsAtIndexes_toPasteboardSelector = mkSelector "imageBrowser:writeItemsAtIndexes:toPasteboard:"

-- | @Selector@ for @numberOfGroupsInImageBrowser:@
numberOfGroupsInImageBrowserSelector :: Selector
numberOfGroupsInImageBrowserSelector = mkSelector "numberOfGroupsInImageBrowser:"

-- | @Selector@ for @imageBrowser:groupAtIndex:@
imageBrowser_groupAtIndexSelector :: Selector
imageBrowser_groupAtIndexSelector = mkSelector "imageBrowser:groupAtIndex:"

-- | @Selector@ for @quartzFilterManager:didAddFilter:@
quartzFilterManager_didAddFilterSelector :: Selector
quartzFilterManager_didAddFilterSelector = mkSelector "quartzFilterManager:didAddFilter:"

-- | @Selector@ for @quartzFilterManager:didRemoveFilter:@
quartzFilterManager_didRemoveFilterSelector :: Selector
quartzFilterManager_didRemoveFilterSelector = mkSelector "quartzFilterManager:didRemoveFilter:"

-- | @Selector@ for @quartzFilterManager:didModifyFilter:@
quartzFilterManager_didModifyFilterSelector :: Selector
quartzFilterManager_didModifyFilterSelector = mkSelector "quartzFilterManager:didModifyFilter:"

-- | @Selector@ for @quartzFilterManager:didSelectFilter:@
quartzFilterManager_didSelectFilterSelector :: Selector
quartzFilterManager_didSelectFilterSelector = mkSelector "quartzFilterManager:didSelectFilter:"

-- | @Selector@ for @compositionPickerView:didSelectComposition:@
compositionPickerView_didSelectCompositionSelector :: Selector
compositionPickerView_didSelectCompositionSelector = mkSelector "compositionPickerView:didSelectComposition:"

-- | @Selector@ for @compositionPickerViewDidStartAnimating:@
compositionPickerViewDidStartAnimatingSelector :: Selector
compositionPickerViewDidStartAnimatingSelector = mkSelector "compositionPickerViewDidStartAnimating:"

-- | @Selector@ for @compositionPickerViewWillStopAnimating:@
compositionPickerViewWillStopAnimatingSelector :: Selector
compositionPickerViewWillStopAnimatingSelector = mkSelector "compositionPickerViewWillStopAnimating:"

-- | @Selector@ for @compositionParameterView:shouldDisplayParameterWithKey:attributes:@
compositionParameterView_shouldDisplayParameterWithKey_attributesSelector :: Selector
compositionParameterView_shouldDisplayParameterWithKey_attributesSelector = mkSelector "compositionParameterView:shouldDisplayParameterWithKey:attributes:"

-- | @Selector@ for @compositionParameterView:didChangeParameterWithKey:@
compositionParameterView_didChangeParameterWithKeySelector :: Selector
compositionParameterView_didChangeParameterWithKeySelector = mkSelector "compositionParameterView:didChangeParameterWithKey:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector
selectableSelector = mkSelector "selectable"

