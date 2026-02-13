{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activeSelector
  , addObserverSelector
  , defaultConfigurationSelector
  , initSelector
  , maximumStreamCountSelector
  , newSelector
  , presentPickerForStreamSelector
  , presentPickerForStream_usingContentStyleSelector
  , presentPickerUsingContentStyleSelector
  , presentSelector
  , removeObserverSelector
  , setActiveSelector
  , setConfiguration_forStreamSelector
  , setDefaultConfigurationSelector
  , setMaximumStreamCountSelector
  , sharedPickerSelector

  -- * Enum types
  , SCShareableContentStyle(SCShareableContentStyle)
  , pattern SCShareableContentStyleNone
  , pattern SCShareableContentStyleWindow
  , pattern SCShareableContentStyleDisplay
  , pattern SCShareableContentStyleApplication

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO (Id SCContentSharingPicker)
init_ scContentSharingPicker =
  sendOwnedMessage scContentSharingPicker initSelector

-- | @+ new@
new :: IO (Id SCContentSharingPicker)
new  =
  do
    cls' <- getRequiredClass "SCContentSharingPicker"
    sendOwnedClassMessage cls' newSelector

-- | addObserver:
--
-- @observer@ — the observer object that adheres to SCContentSharingPickerObserver protocol
--
-- Adds an observer object that will receive the results of user interaction with a displayed picker
--
-- ObjC selector: @- addObserver:@
addObserver :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> RawId -> IO ()
addObserver scContentSharingPicker observer =
  sendMessage scContentSharingPicker addObserverSelector observer

-- | removeObserver:
--
-- @observer@ — the observer object that adheres to SCContentSharingPickerObserver protocol
--
-- Removes an observer object that will receive the results of user interaction with a displayed picker
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> RawId -> IO ()
removeObserver scContentSharingPicker observer =
  sendMessage scContentSharingPicker removeObserverSelector observer

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
setConfiguration_forStream scContentSharingPicker pickerConfig stream =
  sendMessage scContentSharingPicker setConfiguration_forStreamSelector (toSCContentSharingPickerConfiguration pickerConfig) (toSCStream stream)

-- | present
--
-- show content sharing picker to get content for updating a new stream
--
-- ObjC selector: @- present@
present :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO ()
present scContentSharingPicker =
  sendMessage scContentSharingPicker presentSelector

-- | presentPickerUsingContentStyle:
--
-- @contentStyle@ — the mode in which picking should start
--
-- Takes a person straight into picking particular windows or displays
--
-- ObjC selector: @- presentPickerUsingContentStyle:@
presentPickerUsingContentStyle :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> SCShareableContentStyle -> IO ()
presentPickerUsingContentStyle scContentSharingPicker contentStyle =
  sendMessage scContentSharingPicker presentPickerUsingContentStyleSelector contentStyle

-- | presentPickerForStream:
--
-- @stream@ — the stream to update
--
-- show content sharing picker with an existing stream
--
-- ObjC selector: @- presentPickerForStream:@
presentPickerForStream :: (IsSCContentSharingPicker scContentSharingPicker, IsSCStream stream) => scContentSharingPicker -> stream -> IO ()
presentPickerForStream scContentSharingPicker stream =
  sendMessage scContentSharingPicker presentPickerForStreamSelector (toSCStream stream)

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
presentPickerForStream_usingContentStyle scContentSharingPicker stream contentStyle =
  sendMessage scContentSharingPicker presentPickerForStream_usingContentStyleSelector (toSCStream stream) contentStyle

-- | sharedPicker the singleton shared picker for the application
--
-- ObjC selector: @+ sharedPicker@
sharedPicker :: IO (Id SCContentSharingPicker)
sharedPicker  =
  do
    cls' <- getRequiredClass "SCContentSharingPicker"
    sendClassMessage cls' sharedPickerSelector

-- | defaultConfiguration for the content sharing picker. If a stream does not have a configuration, the default configuration will be used.
--
-- ObjC selector: @- defaultConfiguration@
defaultConfiguration :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO (Id SCContentSharingPickerConfiguration)
defaultConfiguration scContentSharingPicker =
  sendMessage scContentSharingPicker defaultConfigurationSelector

-- | defaultConfiguration for the content sharing picker. If a stream does not have a configuration, the default configuration will be used.
--
-- ObjC selector: @- setDefaultConfiguration:@
setDefaultConfiguration :: (IsSCContentSharingPicker scContentSharingPicker, IsSCContentSharingPickerConfiguration value) => scContentSharingPicker -> value -> IO ()
setDefaultConfiguration scContentSharingPicker value =
  sendMessage scContentSharingPicker setDefaultConfigurationSelector (toSCContentSharingPickerConfiguration value)

-- | maximumStreamCount An integer value that, if set, limits when Control Center will show the UI to present a picker with no associated stream. If set to 0, Control Center will never ever show UI to present a picker without an associated stream.
--
-- ObjC selector: @- maximumStreamCount@
maximumStreamCount :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO (Id NSNumber)
maximumStreamCount scContentSharingPicker =
  sendMessage scContentSharingPicker maximumStreamCountSelector

-- | maximumStreamCount An integer value that, if set, limits when Control Center will show the UI to present a picker with no associated stream. If set to 0, Control Center will never ever show UI to present a picker without an associated stream.
--
-- ObjC selector: @- setMaximumStreamCount:@
setMaximumStreamCount :: (IsSCContentSharingPicker scContentSharingPicker, IsNSNumber value) => scContentSharingPicker -> value -> IO ()
setMaximumStreamCount scContentSharingPicker value =
  sendMessage scContentSharingPicker setMaximumStreamCountSelector (toNSNumber value)

-- | active A picker needs to be marked as active for its UI to appear. If @startPickingContent@ is called and the picker is not marked as active, the picker will not appear.
--
-- ObjC selector: @- active@
active :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> IO Bool
active scContentSharingPicker =
  sendMessage scContentSharingPicker activeSelector

-- | active A picker needs to be marked as active for its UI to appear. If @startPickingContent@ is called and the picker is not marked as active, the picker will not appear.
--
-- ObjC selector: @- setActive:@
setActive :: IsSCContentSharingPicker scContentSharingPicker => scContentSharingPicker -> Bool -> IO ()
setActive scContentSharingPicker value =
  sendMessage scContentSharingPicker setActiveSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCContentSharingPicker)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SCContentSharingPicker)
newSelector = mkSelector "new"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector '[RawId] ()
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector '[RawId] ()
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @setConfiguration:forStream:@
setConfiguration_forStreamSelector :: Selector '[Id SCContentSharingPickerConfiguration, Id SCStream] ()
setConfiguration_forStreamSelector = mkSelector "setConfiguration:forStream:"

-- | @Selector@ for @present@
presentSelector :: Selector '[] ()
presentSelector = mkSelector "present"

-- | @Selector@ for @presentPickerUsingContentStyle:@
presentPickerUsingContentStyleSelector :: Selector '[SCShareableContentStyle] ()
presentPickerUsingContentStyleSelector = mkSelector "presentPickerUsingContentStyle:"

-- | @Selector@ for @presentPickerForStream:@
presentPickerForStreamSelector :: Selector '[Id SCStream] ()
presentPickerForStreamSelector = mkSelector "presentPickerForStream:"

-- | @Selector@ for @presentPickerForStream:usingContentStyle:@
presentPickerForStream_usingContentStyleSelector :: Selector '[Id SCStream, SCShareableContentStyle] ()
presentPickerForStream_usingContentStyleSelector = mkSelector "presentPickerForStream:usingContentStyle:"

-- | @Selector@ for @sharedPicker@
sharedPickerSelector :: Selector '[] (Id SCContentSharingPicker)
sharedPickerSelector = mkSelector "sharedPicker"

-- | @Selector@ for @defaultConfiguration@
defaultConfigurationSelector :: Selector '[] (Id SCContentSharingPickerConfiguration)
defaultConfigurationSelector = mkSelector "defaultConfiguration"

-- | @Selector@ for @setDefaultConfiguration:@
setDefaultConfigurationSelector :: Selector '[Id SCContentSharingPickerConfiguration] ()
setDefaultConfigurationSelector = mkSelector "setDefaultConfiguration:"

-- | @Selector@ for @maximumStreamCount@
maximumStreamCountSelector :: Selector '[] (Id NSNumber)
maximumStreamCountSelector = mkSelector "maximumStreamCount"

-- | @Selector@ for @setMaximumStreamCount:@
setMaximumStreamCountSelector :: Selector '[Id NSNumber] ()
setMaximumStreamCountSelector = mkSelector "setMaximumStreamCount:"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Bool] ()
setActiveSelector = mkSelector "setActive:"

