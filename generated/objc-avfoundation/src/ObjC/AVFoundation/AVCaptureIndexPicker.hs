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
  , initWithLocalizedTitle_symbolName_numberOfIndexesSelector
  , initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector
  , initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector
  , setActionQueue_actionSelector
  , selectedIndexSelector
  , setSelectedIndexSelector
  , localizedTitleSelector
  , symbolNameSelector
  , numberOfIndexesSelector
  , localizedIndexTitlesSelector
  , accessibilityIdentifierSelector
  , setAccessibilityIdentifierSelector


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
initWithLocalizedTitle_symbolName_numberOfIndexes avCaptureIndexPicker  localizedTitle symbolName numberOfIndexes =
withObjCPtr localizedTitle $ \raw_localizedTitle ->
  withObjCPtr symbolName $ \raw_symbolName ->
      sendMsg avCaptureIndexPicker (mkSelector "initWithLocalizedTitle:symbolName:numberOfIndexes:") (retPtr retVoid) [argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_symbolName :: Ptr ()), argCLong (fromIntegral numberOfIndexes)] >>= ownedObject . castPtr

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
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransform avCaptureIndexPicker  localizedTitle symbolName numberOfIndexes localizedTitleTransform =
withObjCPtr localizedTitle $ \raw_localizedTitle ->
  withObjCPtr symbolName $ \raw_symbolName ->
      sendMsg avCaptureIndexPicker (mkSelector "initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:") (retPtr retVoid) [argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_symbolName :: Ptr ()), argCLong (fromIntegral numberOfIndexes), argPtr (castPtr localizedTitleTransform :: Ptr ())] >>= ownedObject . castPtr

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
initWithLocalizedTitle_symbolName_localizedIndexTitles avCaptureIndexPicker  localizedTitle symbolName localizedIndexTitles =
withObjCPtr localizedTitle $ \raw_localizedTitle ->
  withObjCPtr symbolName $ \raw_symbolName ->
    withObjCPtr localizedIndexTitles $ \raw_localizedIndexTitles ->
        sendMsg avCaptureIndexPicker (mkSelector "initWithLocalizedTitle:symbolName:localizedIndexTitles:") (retPtr retVoid) [argPtr (castPtr raw_localizedTitle :: Ptr ()), argPtr (castPtr raw_symbolName :: Ptr ()), argPtr (castPtr raw_localizedIndexTitles :: Ptr ())] >>= ownedObject . castPtr

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
setActionQueue_action avCaptureIndexPicker  actionQueue action =
withObjCPtr actionQueue $ \raw_actionQueue ->
    sendMsg avCaptureIndexPicker (mkSelector "setActionQueue:action:") retVoid [argPtr (castPtr raw_actionQueue :: Ptr ()), argPtr (castPtr action :: Ptr ())]

-- | selectedIndex
--
-- The currently selected index.
--
-- Because the camera system may be independent from the main thread or `@, @selectedIndex@ must be changed on @actionQueue@ – the queue provided to @setActionQueue:action:@. The default value is 0. An index may only be set if it is greater than 0 or less than @numberOfIndexes@, otherwise an @NSInvalidArgumentException` is thrown.
--
-- ObjC selector: @- selectedIndex@
selectedIndex :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO CLong
selectedIndex avCaptureIndexPicker  =
  sendMsg avCaptureIndexPicker (mkSelector "selectedIndex") retCLong []

-- | selectedIndex
--
-- The currently selected index.
--
-- Because the camera system may be independent from the main thread or `@, @selectedIndex@ must be changed on @actionQueue@ – the queue provided to @setActionQueue:action:@. The default value is 0. An index may only be set if it is greater than 0 or less than @numberOfIndexes@, otherwise an @NSInvalidArgumentException` is thrown.
--
-- ObjC selector: @- setSelectedIndex:@
setSelectedIndex :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> CLong -> IO ()
setSelectedIndex avCaptureIndexPicker  value =
  sendMsg avCaptureIndexPicker (mkSelector "setSelectedIndex:") retVoid [argCLong (fromIntegral value)]

-- | localizedTitle
--
-- A localized string that describes the picker's @action@.
--
-- ObjC selector: @- localizedTitle@
localizedTitle :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSString)
localizedTitle avCaptureIndexPicker  =
  sendMsg avCaptureIndexPicker (mkSelector "localizedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | symbolName
--
-- The name of a symbol to represent the picker.
--
-- ObjC selector: @- symbolName@
symbolName :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSString)
symbolName avCaptureIndexPicker  =
  sendMsg avCaptureIndexPicker (mkSelector "symbolName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | numberOfIndexes
--
-- The number of indexes to pick between.
--
-- ObjC selector: @- numberOfIndexes@
numberOfIndexes :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO CLong
numberOfIndexes avCaptureIndexPicker  =
  sendMsg avCaptureIndexPicker (mkSelector "numberOfIndexes") retCLong []

-- | localizedIndexTitles
--
-- The titles used for each index.
--
-- ObjC selector: @- localizedIndexTitles@
localizedIndexTitles :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSArray)
localizedIndexTitles avCaptureIndexPicker  =
  sendMsg avCaptureIndexPicker (mkSelector "localizedIndexTitles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | accessibilityIdentifier
--
-- A string that identifies the picker.
--
-- ObjC selector: @- accessibilityIdentifier@
accessibilityIdentifier :: IsAVCaptureIndexPicker avCaptureIndexPicker => avCaptureIndexPicker -> IO (Id NSString)
accessibilityIdentifier avCaptureIndexPicker  =
  sendMsg avCaptureIndexPicker (mkSelector "accessibilityIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | accessibilityIdentifier
--
-- A string that identifies the picker.
--
-- ObjC selector: @- setAccessibilityIdentifier:@
setAccessibilityIdentifier :: (IsAVCaptureIndexPicker avCaptureIndexPicker, IsNSString value) => avCaptureIndexPicker -> value -> IO ()
setAccessibilityIdentifier avCaptureIndexPicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptureIndexPicker (mkSelector "setAccessibilityIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedTitle:symbolName:numberOfIndexes:@
initWithLocalizedTitle_symbolName_numberOfIndexesSelector :: Selector
initWithLocalizedTitle_symbolName_numberOfIndexesSelector = mkSelector "initWithLocalizedTitle:symbolName:numberOfIndexes:"

-- | @Selector@ for @initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:@
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector :: Selector
initWithLocalizedTitle_symbolName_numberOfIndexes_localizedTitleTransformSelector = mkSelector "initWithLocalizedTitle:symbolName:numberOfIndexes:localizedTitleTransform:"

-- | @Selector@ for @initWithLocalizedTitle:symbolName:localizedIndexTitles:@
initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector :: Selector
initWithLocalizedTitle_symbolName_localizedIndexTitlesSelector = mkSelector "initWithLocalizedTitle:symbolName:localizedIndexTitles:"

-- | @Selector@ for @setActionQueue:action:@
setActionQueue_actionSelector :: Selector
setActionQueue_actionSelector = mkSelector "setActionQueue:action:"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @symbolName@
symbolNameSelector :: Selector
symbolNameSelector = mkSelector "symbolName"

-- | @Selector@ for @numberOfIndexes@
numberOfIndexesSelector :: Selector
numberOfIndexesSelector = mkSelector "numberOfIndexes"

-- | @Selector@ for @localizedIndexTitles@
localizedIndexTitlesSelector :: Selector
localizedIndexTitlesSelector = mkSelector "localizedIndexTitles"

-- | @Selector@ for @accessibilityIdentifier@
accessibilityIdentifierSelector :: Selector
accessibilityIdentifierSelector = mkSelector "accessibilityIdentifier"

-- | @Selector@ for @setAccessibilityIdentifier:@
setAccessibilityIdentifierSelector :: Selector
setAccessibilityIdentifierSelector = mkSelector "setAccessibilityIdentifier:"

