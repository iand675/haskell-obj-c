{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCContentSharingPicker
--
-- SCContentSharingPicker is an object created by client applications to opt-in to Control Center's content picker
--
-- Generated bindings for @SCContentSharingPicker@.
module ObjC.ScreenCaptureKit.SCContentSharingPicker
  ( SCContentSharingPicker
  , IsSCContentSharingPicker(..)
  , init_
  , new
  , addObserver
  , removeObserver
  , setConfiguration_forStream
  , present
  , presentPickerUsingContentStyle
  , presentPickerForStream
  , presentPickerForStream_usingContentStyle
  , sharedPicker
  , defaultConfiguration
  , setDefaultConfiguration
  , maximumStreamCount
  , setMaximumStreamCount
  , active
  , setActive
  , initSelector
  , newSelector
  , addObserverSelector
  , removeObserverSelector
  , setConfiguration_forStreamSelector
  , presentSelector
  , presentPickerUsingContentStyleSelector
  , presentPickerForStreamSelector
  , presentPickerForStream_usingContentStyleSelector
  , sharedPickerSelector
  , defaultConfigurationSelector
  , setDefaultConfigurationSelector
  , maximumStreamCountSelector
  , setMaximumStreamCountSelector
  , activeSelector
  , setActiveSelector

  -- * Enum types
  , SCShareableContentStyle(SCShareableContentStyle)
  , pattern SCShareableContentStyleNone
  , pattern SCShareableContentStyleWindow
  , pattern SCShareableContentStyleDisplay
  , pattern SCShareableContentStyleApplication

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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO (Id SCContentSharingPicker)
init_ scContentSharingPicker  =
    sendMsg scContentSharingPicker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SCContentSharingPicker)
new  =
  do
    cls' <- getRequiredClass "SCContentSharingPicker"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | addObserver:
--
-- @observer@ — the observer object that adheres to SCContentSharingPickerObserver protocol
--
-- Adds an observer object that will receive the results of user interaction with a displayed picker
--
-- ObjC selector: @- addObserver:@
addObserver :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> RawId -> IO ()
addObserver scContentSharingPicker  observer =
    sendMsg scContentSharingPicker (mkSelector "addObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | removeObserver:
--
-- @observer@ — the observer object that adheres to SCContentSharingPickerObserver protocol
--
-- Removes an observer object that will receive the results of user interaction with a displayed picker
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> RawId -> IO ()
removeObserver scContentSharingPicker  observer =
    sendMsg scContentSharingPicker (mkSelector "removeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | setConfiguration:forStream:
--
-- @pickerConfig@ — configuration for the picker
--
-- @stream@ — stream for optional picking configuration
--
-- Sets optional configuration for the picker for a specific stream. If this is not set, the stream will use the defaultConfiguration instead
--
-- ObjC selector: @- setConfiguration:forStream:@
setConfiguration_forStream :: (IsSCContentSharingPicker scContentSharingPicker, IsSCContentSharingPickerConfiguration pickerConfig, IsSCStream stream) => scContentSharingPicker -> pickerConfig -> stream -> IO ()
setConfiguration_forStream scContentSharingPicker  pickerConfig stream =
  withObjCPtr pickerConfig $ \raw_pickerConfig ->
    withObjCPtr stream $ \raw_stream ->
        sendMsg scContentSharingPicker (mkSelector "setConfiguration:forStream:") retVoid [argPtr (castPtr raw_pickerConfig :: Ptr ()), argPtr (castPtr raw_stream :: Ptr ())]

-- | present
--
-- show content sharing picker to get content for updating a new stream
--
-- ObjC selector: @- present@
present :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO ()
present scContentSharingPicker  =
    sendMsg scContentSharingPicker (mkSelector "present") retVoid []

-- | presentPickerUsingContentStyle:
--
-- @contentStyle@ — the mode in which picking should start
--
-- Takes a person straight into picking particular windows or displays
--
-- ObjC selector: @- presentPickerUsingContentStyle:@
presentPickerUsingContentStyle :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> SCShareableContentStyle -> IO ()
presentPickerUsingContentStyle scContentSharingPicker  contentStyle =
    sendMsg scContentSharingPicker (mkSelector "presentPickerUsingContentStyle:") retVoid [argCLong (coerce contentStyle)]

-- | presentPickerForStream:
--
-- @stream@ — the stream to update
--
-- show content sharing picker with an existing stream
--
-- ObjC selector: @- presentPickerForStream:@
presentPickerForStream :: (IsSCContentSharingPicker scContentSharingPicker, IsSCStream stream) => scContentSharingPicker -> stream -> IO ()
presentPickerForStream scContentSharingPicker  stream =
  withObjCPtr stream $ \raw_stream ->
      sendMsg scContentSharingPicker (mkSelector "presentPickerForStream:") retVoid [argPtr (castPtr raw_stream :: Ptr ())]

-- | presentPickerForStream:usingContentStyle:
--
-- @stream@ — the stream that the picker will display
--
-- @contentStyle@ — the mode in which picking should start
--
-- Takes a person straight into picking particular windows or displays
--
-- ObjC selector: @- presentPickerForStream:usingContentStyle:@
presentPickerForStream_usingContentStyle :: (IsSCContentSharingPicker scContentSharingPicker, IsSCStream stream) => scContentSharingPicker -> stream -> SCShareableContentStyle -> IO ()
presentPickerForStream_usingContentStyle scContentSharingPicker  stream contentStyle =
  withObjCPtr stream $ \raw_stream ->
      sendMsg scContentSharingPicker (mkSelector "presentPickerForStream:usingContentStyle:") retVoid [argPtr (castPtr raw_stream :: Ptr ()), argCLong (coerce contentStyle)]

-- | sharedPicker the singleton shared picker for the application
--
-- ObjC selector: @+ sharedPicker@
sharedPicker :: IO (Id SCContentSharingPicker)
sharedPicker  =
  do
    cls' <- getRequiredClass "SCContentSharingPicker"
    sendClassMsg cls' (mkSelector "sharedPicker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultConfiguration for the content sharing picker. If a stream does not have a configuration, the default configuration will be used.
--
-- ObjC selector: @- defaultConfiguration@
defaultConfiguration :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO (Id SCContentSharingPickerConfiguration)
defaultConfiguration scContentSharingPicker  =
    sendMsg scContentSharingPicker (mkSelector "defaultConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultConfiguration for the content sharing picker. If a stream does not have a configuration, the default configuration will be used.
--
-- ObjC selector: @- setDefaultConfiguration:@
setDefaultConfiguration :: (IsSCContentSharingPicker scContentSharingPicker, IsSCContentSharingPickerConfiguration value) => scContentSharingPicker -> value -> IO ()
setDefaultConfiguration scContentSharingPicker  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scContentSharingPicker (mkSelector "setDefaultConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | maximumStreamCount An integer value that, if set, limits when Control Center will show the UI to present a picker with no associated stream. If set to 0, Control Center will never ever show UI to present a picker without an associated stream.
--
-- ObjC selector: @- maximumStreamCount@
maximumStreamCount :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO (Id NSNumber)
maximumStreamCount scContentSharingPicker  =
    sendMsg scContentSharingPicker (mkSelector "maximumStreamCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maximumStreamCount An integer value that, if set, limits when Control Center will show the UI to present a picker with no associated stream. If set to 0, Control Center will never ever show UI to present a picker without an associated stream.
--
-- ObjC selector: @- setMaximumStreamCount:@
setMaximumStreamCount :: (IsSCContentSharingPicker scContentSharingPicker, IsNSNumber value) => scContentSharingPicker -> value -> IO ()
setMaximumStreamCount scContentSharingPicker  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scContentSharingPicker (mkSelector "setMaximumStreamCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | active A picker needs to be marked as active for its UI to appear. If @startPickingContent@ is called and the picker is not marked as active, the picker will not appear.
--
-- ObjC selector: @- active@
active :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO Bool
active scContentSharingPicker  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scContentSharingPicker (mkSelector "active") retCULong []

-- | active A picker needs to be marked as active for its UI to appear. If @startPickingContent@ is called and the picker is not marked as active, the picker will not appear.
--
-- ObjC selector: @- setActive:@
setActive :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> Bool -> IO ()
setActive scContentSharingPicker  value =
    sendMsg scContentSharingPicker (mkSelector "setActive:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @setConfiguration:forStream:@
setConfiguration_forStreamSelector :: Selector
setConfiguration_forStreamSelector = mkSelector "setConfiguration:forStream:"

-- | @Selector@ for @present@
presentSelector :: Selector
presentSelector = mkSelector "present"

-- | @Selector@ for @presentPickerUsingContentStyle:@
presentPickerUsingContentStyleSelector :: Selector
presentPickerUsingContentStyleSelector = mkSelector "presentPickerUsingContentStyle:"

-- | @Selector@ for @presentPickerForStream:@
presentPickerForStreamSelector :: Selector
presentPickerForStreamSelector = mkSelector "presentPickerForStream:"

-- | @Selector@ for @presentPickerForStream:usingContentStyle:@
presentPickerForStream_usingContentStyleSelector :: Selector
presentPickerForStream_usingContentStyleSelector = mkSelector "presentPickerForStream:usingContentStyle:"

-- | @Selector@ for @sharedPicker@
sharedPickerSelector :: Selector
sharedPickerSelector = mkSelector "sharedPicker"

-- | @Selector@ for @defaultConfiguration@
defaultConfigurationSelector :: Selector
defaultConfigurationSelector = mkSelector "defaultConfiguration"

-- | @Selector@ for @setDefaultConfiguration:@
setDefaultConfigurationSelector :: Selector
setDefaultConfigurationSelector = mkSelector "setDefaultConfiguration:"

-- | @Selector@ for @maximumStreamCount@
maximumStreamCountSelector :: Selector
maximumStreamCountSelector = mkSelector "maximumStreamCount"

-- | @Selector@ for @setMaximumStreamCount:@
setMaximumStreamCountSelector :: Selector
setMaximumStreamCountSelector = mkSelector "setMaximumStreamCount:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

