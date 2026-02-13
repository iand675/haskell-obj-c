{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureIndexPicker
--
-- An @AVCaptureControl@ for selecting from a set of mutually exclusive values by index.
--
-- @AVCaptureIndexPicker@ is ideal when the set of values is provided by an indexed container like @NSArray@, @Array@, or @Sequence@. Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- @AVCaptureIndexPicker@ uses zero-based indexing.
--
-- Generated bindings for @AVCaptureIndexPicker@.
module ObjC.AVFoundation.AVCaptureIndexPicker
  ( AVCaptureIndexPicker
  , IsAVCaptureIndexPicker(..)
  , initWithLocalizedTitle_symbolName_numberOfIndexes
  , initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransform
  , initWithLocalizedTitle_symbolName_localizedIndexTitles
  , setActionQueue_action
  , selectedIndex
  , setSelectedIndex
  , localizedTitle
  , symbolName
  , numberOfIndexes
  , localizedIndexTitles
  , accessibilityIdentifier
  , setAccessibilityIdentifier
  , accessibilityIdentifierSelector
  , initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector
  , initWithLocalizedTitle_symbolName_numberOfIndexesSelector
  , initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector
  , localizedIndexTitlesSelector
  , localizedTitleSelector
  , numberOfIndexesSelector
  , selectedIndexSelector
  , setAccessibilityIdentifierSelector
  , setActionQueue_actionSelector
  , setSelectedIndexSelector
  , symbolNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithLocalizedTitle:symbolName:numberOfIndexes:
--
-- Initializes an @AVCaptureIndexPicker@ to pick between @numberOfIndexes@ values.
--
-- @localizedTitle@ — A localized string that describes the picker's @action@.
--
-- @symbolName@ — The name of a symbol to represent the picker.
--
-- @numberOfIndexes@ — The number of indexes to pick between. @numberOfIndexes@ must be greater than 0, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- Returns: An @AVCaptureIndexPicker@ instance that picks between @numberOfIndexes@ values.
--
-- Suitable when your picked values don't need titles.
--
-- ObjC selector: @- initWithLocalizedTitle:symbolName:numberOfIndexes:@
initWithLocalizedTitle_symbolName_numberOfIndexes :: (IsAVCaptureIndexPicker avCaptureIndexPicker, IsNSString localizedTitle, IsNSString symbolName) => avCaptureIndexPicker -> localizedTitle -> symbolName -> CLong -> IO (Id AVCaptureIndexPicker)
initWithLocalizedTitle_symbolName_numberOfIndexes avCaptureIndexPicker localizedTitle symbolName numberOfIndexes =
  sendOwnedMessage avCaptureIndexPicker initWithLocalizedTitle_symbolName_numberOfIndexesSelector (toNSString localizedTitle) (toNSString symbolName) numberOfIndexes

-- | initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:
--
-- Initializes an @AVCaptureIndexPicker@ to pick between @numberOfIndexes@ values.
--
-- @localizedTitle@ — A localized string that describes the picker's @action@.
--
-- @symbolName@ — The name of a symbol to represent the picker.
--
-- @numberOfIndexes@ — The number of indexes to pick between. @numberOfIndexes@ must be greater than 0, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- @localizedTitleTransform@ — A transformation from index to localized title.
--
-- Returns: An @AVCaptureIndexPicker@ instance that picks between @numberOfIndexes@ values with a transformation from index to localized title.
--
-- Suitable when you want to provide a title for each picked value lazily.
--
-- ObjC selector: @- initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:@
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransform :: (IsAVCaptureIndexPicker avCaptureIndexPicker, IsNSString localizedTitle, IsNSString symbolName) => avCaptureIndexPicker -> localizedTitle -> symbolName -> CLong -> Ptr () -> IO (Id AVCaptureIndexPicker)
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransform avCaptureIndexPicker localizedTitle symbolName numberOfIndexes localizedTitleTransform =
  sendOwnedMessage avCaptureIndexPicker initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector (toNSString localizedTitle) (toNSString symbolName) numberOfIndexes localizedTitleTransform

-- | initWithLocalizedTitle:symbolName:localizedIndexTitles:
--
-- Initializes an @AVCaptureIndexPicker@ to pick between @localizedIndexTitles.count@ values.
--
-- @localizedTitle@ — A localized string that describes the picker's @action@.
--
-- @symbolName@ — The name of a symbol to represent the picker.
--
-- @localizedIndexTitles@ — The titles to use for each index. @localizedIndexTitles@ must be greater than 0, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- Returns: An @AVCaptureIndexPicker@ instance that picks between @localizedIndexTitles.count@ values.
--
-- Suitable when you already have an array containing a title for each picked value.
--
-- ObjC selector: @- initWithLocalizedTitle:symbolName:localizedIndexTitles:@
initWithLocalizedTitle_symbolName_localizedIndexTitles :: (IsAVCaptureIndexPicker avCaptureIndexPicker, IsNSString localizedTitle, IsNSString symbolName, IsNSArray localizedIndexTitles) => avCaptureIndexPicker -> localizedTitle -> symbolName -> localizedIndexTitles -> IO (Id AVCaptureIndexPicker)
initWithLocalizedTitle_symbolName_localizedIndexTitles avCaptureIndexPicker localizedTitle symbolName localizedIndexTitles =
  sendOwnedMessage avCaptureIndexPicker initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector (toNSString localizedTitle) (toNSString symbolName) (toNSArray localizedIndexTitles)

-- | setActionQueue:action:
--
-- Configures the picker's @action@ which is called on @actionQueue@ whenever the index of the picker is changed.
--
-- @actionQueue@ — A queue for the @action@ to be called.
--
-- @action@ — An action called on @actionQueue@ whenever the selected index of the picker is changed.
--
-- Because the camera system may be independent from the main thread or `@, @action@ is always called on an internal @DispatchSerialQueue@ targeted at @actionQueue`.
--
-- If @action@ modifies a property of the camera system, @actionQueue@ must represent the same exclusive execution context as the camera system (see @isSameExclusiveExecutionContext@).
--
-- ObjC selector: @- setActionQueue:action:@
setActionQueue_action :: (IsAVCaptureIndexPicker avCaptureIndexPicker, IsNSObject actionQueue) => avCaptureIndexPicker -> actionQueue -> Ptr () -> IO ()
setActionQueue_action avCaptureIndexPicker actionQueue action =
  sendMessage avCaptureIndexPicker setActionQueue_actionSelector (toNSObject actionQueue) action

-- | selectedIndex
--
-- The currently selected index.
--
-- Because the camera system may be independent from the main thread or `@, @selectedIndex@ must be changed on @actionQueue@ – the queue provided to @setActionQueue:action:@. The default value is 0. An index may only be set if it is greater than 0 or less than @numberOfIndexes@, otherwise an @NSInvalidArgumentException` is thrown.
--
-- ObjC selector: @- selectedIndex@
selectedIndex :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO CLong
selectedIndex avCaptureIndexPicker =
  sendMessage avCaptureIndexPicker selectedIndexSelector

-- | selectedIndex
--
-- The currently selected index.
--
-- Because the camera system may be independent from the main thread or `@, @selectedIndex@ must be changed on @actionQueue@ – the queue provided to @setActionQueue:action:@. The default value is 0. An index may only be set if it is greater than 0 or less than @numberOfIndexes@, otherwise an @NSInvalidArgumentException` is thrown.
--
-- ObjC selector: @- setSelectedIndex:@
setSelectedIndex :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> CLong -> IO ()
setSelectedIndex avCaptureIndexPicker value =
  sendMessage avCaptureIndexPicker setSelectedIndexSelector value

-- | localizedTitle
--
-- A localized string that describes the picker's @action@.
--
-- ObjC selector: @- localizedTitle@
localizedTitle :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSString)
localizedTitle avCaptureIndexPicker =
  sendMessage avCaptureIndexPicker localizedTitleSelector

-- | symbolName
--
-- The name of a symbol to represent the picker.
--
-- ObjC selector: @- symbolName@
symbolName :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSString)
symbolName avCaptureIndexPicker =
  sendMessage avCaptureIndexPicker symbolNameSelector

-- | numberOfIndexes
--
-- The number of indexes to pick between.
--
-- ObjC selector: @- numberOfIndexes@
numberOfIndexes :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO CLong
numberOfIndexes avCaptureIndexPicker =
  sendMessage avCaptureIndexPicker numberOfIndexesSelector

-- | localizedIndexTitles
--
-- The titles used for each index.
--
-- ObjC selector: @- localizedIndexTitles@
localizedIndexTitles :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSArray)
localizedIndexTitles avCaptureIndexPicker =
  sendMessage avCaptureIndexPicker localizedIndexTitlesSelector

-- | accessibilityIdentifier
--
-- A string that identifies the picker.
--
-- ObjC selector: @- accessibilityIdentifier@
accessibilityIdentifier :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSString)
accessibilityIdentifier avCaptureIndexPicker =
  sendMessage avCaptureIndexPicker accessibilityIdentifierSelector

-- | accessibilityIdentifier
--
-- A string that identifies the picker.
--
-- ObjC selector: @- setAccessibilityIdentifier:@
setAccessibilityIdentifier :: (IsAVCaptureIndexPicker avCaptureIndexPicker, IsNSString value) => avCaptureIndexPicker -> value -> IO ()
setAccessibilityIdentifier avCaptureIndexPicker value =
  sendMessage avCaptureIndexPicker setAccessibilityIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedTitle:symbolName:numberOfIndexes:@
initWithLocalizedTitle_symbolName_numberOfIndexesSelector :: Selector '[Id NSString, Id NSString, CLong] (Id AVCaptureIndexPicker)
initWithLocalizedTitle_symbolName_numberOfIndexesSelector = mkSelector "initWithLocalizedTitle:symbolName:numberOfIndexes:"

-- | @Selector@ for @initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:@
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector :: Selector '[Id NSString, Id NSString, CLong, Ptr ()] (Id AVCaptureIndexPicker)
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector = mkSelector "initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:"

-- | @Selector@ for @initWithLocalizedTitle:symbolName:localizedIndexTitles:@
initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector :: Selector '[Id NSString, Id NSString, Id NSArray] (Id AVCaptureIndexPicker)
initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector = mkSelector "initWithLocalizedTitle:symbolName:localizedIndexTitles:"

-- | @Selector@ for @setActionQueue:action:@
setActionQueue_actionSelector :: Selector '[Id NSObject, Ptr ()] ()
setActionQueue_actionSelector = mkSelector "setActionQueue:action:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CLong
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CLong] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector '[] (Id NSString)
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @symbolName@
symbolNameSelector :: Selector '[] (Id NSString)
symbolNameSelector = mkSelector "symbolName"

-- | @Selector@ for @numberOfIndexes@
numberOfIndexesSelector :: Selector '[] CLong
numberOfIndexesSelector = mkSelector "numberOfIndexes"

-- | @Selector@ for @localizedIndexTitles@
localizedIndexTitlesSelector :: Selector '[] (Id NSArray)
localizedIndexTitlesSelector = mkSelector "localizedIndexTitles"

-- | @Selector@ for @accessibilityIdentifier@
accessibilityIdentifierSelector :: Selector '[] (Id NSString)
accessibilityIdentifierSelector = mkSelector "accessibilityIdentifier"

-- | @Selector@ for @setAccessibilityIdentifier:@
setAccessibilityIdentifierSelector :: Selector '[Id NSString] ()
setAccessibilityIdentifierSelector = mkSelector "setAccessibilityIdentifier:"

