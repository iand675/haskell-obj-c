{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextInputContext@.
module ObjC.AppKit.NSTextInputContext
  ( NSTextInputContext
  , IsNSTextInputContext(..)
  , initWithClient
  , init_
  , activate
  , deactivate
  , handleEvent
  , discardMarkedText
  , invalidateCharacterCoordinates
  , textInputClientWillStartScrollingOrZooming
  , textInputClientDidEndScrollingOrZooming
  , textInputClientDidUpdateSelection
  , textInputClientDidScroll
  , localizedNameForInputSource
  , currentInputContext
  , client
  , acceptsGlyphInfo
  , setAcceptsGlyphInfo
  , allowedInputSourceLocales
  , setAllowedInputSourceLocales
  , keyboardInputSources
  , selectedKeyboardInputSource
  , setSelectedKeyboardInputSource
  , acceptsGlyphInfoSelector
  , activateSelector
  , allowedInputSourceLocalesSelector
  , clientSelector
  , currentInputContextSelector
  , deactivateSelector
  , discardMarkedTextSelector
  , handleEventSelector
  , initSelector
  , initWithClientSelector
  , invalidateCharacterCoordinatesSelector
  , keyboardInputSourcesSelector
  , localizedNameForInputSourceSelector
  , selectedKeyboardInputSourceSelector
  , setAcceptsGlyphInfoSelector
  , setAllowedInputSourceLocalesSelector
  , setSelectedKeyboardInputSourceSelector
  , textInputClientDidEndScrollingOrZoomingSelector
  , textInputClientDidScrollSelector
  , textInputClientDidUpdateSelectionSelector
  , textInputClientWillStartScrollingOrZoomingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithClient:@
initWithClient :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> RawId -> IO (Id NSTextInputContext)
initWithClient nsTextInputContext client =
  sendOwnedMessage nsTextInputContext initWithClientSelector client

-- | @- init@
init_ :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSTextInputContext)
init_ nsTextInputContext =
  sendOwnedMessage nsTextInputContext initSelector

-- | ** Activation ****
--
-- ObjC selector: @- activate@
activate :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
activate nsTextInputContext =
  sendMessage nsTextInputContext activateSelector

-- | @- deactivate@
deactivate :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
deactivate nsTextInputContext =
  sendMessage nsTextInputContext deactivateSelector

-- | ** Input source interface ***
--
-- ObjC selector: @- handleEvent:@
handleEvent :: (IsNSTextInputContext nsTextInputContext, IsNSEvent event) => nsTextInputContext -> event -> IO Bool
handleEvent nsTextInputContext event =
  sendMessage nsTextInputContext handleEventSelector (toNSEvent event)

-- | @- discardMarkedText@
discardMarkedText :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
discardMarkedText nsTextInputContext =
  sendMessage nsTextInputContext discardMarkedTextSelector

-- | @- invalidateCharacterCoordinates@
invalidateCharacterCoordinates :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
invalidateCharacterCoordinates nsTextInputContext =
  sendMessage nsTextInputContext invalidateCharacterCoordinatesSelector

-- | @- textInputClientWillStartScrollingOrZooming@
textInputClientWillStartScrollingOrZooming :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientWillStartScrollingOrZooming nsTextInputContext =
  sendMessage nsTextInputContext textInputClientWillStartScrollingOrZoomingSelector

-- | @- textInputClientDidEndScrollingOrZooming@
textInputClientDidEndScrollingOrZooming :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientDidEndScrollingOrZooming nsTextInputContext =
  sendMessage nsTextInputContext textInputClientDidEndScrollingOrZoomingSelector

-- | @- textInputClientDidUpdateSelection@
textInputClientDidUpdateSelection :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientDidUpdateSelection nsTextInputContext =
  sendMessage nsTextInputContext textInputClientDidUpdateSelectionSelector

-- | @- textInputClientDidScroll@
textInputClientDidScroll :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientDidScroll nsTextInputContext =
  sendMessage nsTextInputContext textInputClientDidScrollSelector

-- | ** Text Input source attributes ***
--
-- ObjC selector: @+ localizedNameForInputSource:@
localizedNameForInputSource :: IsNSString inputSourceIdentifier => inputSourceIdentifier -> IO (Id NSString)
localizedNameForInputSource inputSourceIdentifier =
  do
    cls' <- getRequiredClass "NSTextInputContext"
    sendClassMessage cls' localizedNameForInputSourceSelector (toNSString inputSourceIdentifier)

-- | @+ currentInputContext@
currentInputContext :: IO (Id NSTextInputContext)
currentInputContext  =
  do
    cls' <- getRequiredClass "NSTextInputContext"
    sendClassMessage cls' currentInputContextSelector

-- | ** Properties ****
--
-- ObjC selector: @- client@
client :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO RawId
client nsTextInputContext =
  sendMessage nsTextInputContext clientSelector

-- | @- acceptsGlyphInfo@
acceptsGlyphInfo :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO Bool
acceptsGlyphInfo nsTextInputContext =
  sendMessage nsTextInputContext acceptsGlyphInfoSelector

-- | @- setAcceptsGlyphInfo:@
setAcceptsGlyphInfo :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> Bool -> IO ()
setAcceptsGlyphInfo nsTextInputContext value =
  sendMessage nsTextInputContext setAcceptsGlyphInfoSelector value

-- | @- allowedInputSourceLocales@
allowedInputSourceLocales :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSArray)
allowedInputSourceLocales nsTextInputContext =
  sendMessage nsTextInputContext allowedInputSourceLocalesSelector

-- | @- setAllowedInputSourceLocales:@
setAllowedInputSourceLocales :: (IsNSTextInputContext nsTextInputContext, IsNSArray value) => nsTextInputContext -> value -> IO ()
setAllowedInputSourceLocales nsTextInputContext value =
  sendMessage nsTextInputContext setAllowedInputSourceLocalesSelector (toNSArray value)

-- | ** Text Input sources handling ***
--
-- ObjC selector: @- keyboardInputSources@
keyboardInputSources :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSArray)
keyboardInputSources nsTextInputContext =
  sendMessage nsTextInputContext keyboardInputSourcesSelector

-- | @- selectedKeyboardInputSource@
selectedKeyboardInputSource :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSString)
selectedKeyboardInputSource nsTextInputContext =
  sendMessage nsTextInputContext selectedKeyboardInputSourceSelector

-- | @- setSelectedKeyboardInputSource:@
setSelectedKeyboardInputSource :: (IsNSTextInputContext nsTextInputContext, IsNSString value) => nsTextInputContext -> value -> IO ()
setSelectedKeyboardInputSource nsTextInputContext value =
  sendMessage nsTextInputContext setSelectedKeyboardInputSourceSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithClient:@
initWithClientSelector :: Selector '[RawId] (Id NSTextInputContext)
initWithClientSelector = mkSelector "initWithClient:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextInputContext)
initSelector = mkSelector "init"

-- | @Selector@ for @activate@
activateSelector :: Selector '[] ()
activateSelector = mkSelector "activate"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector '[] ()
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @handleEvent:@
handleEventSelector :: Selector '[Id NSEvent] Bool
handleEventSelector = mkSelector "handleEvent:"

-- | @Selector@ for @discardMarkedText@
discardMarkedTextSelector :: Selector '[] ()
discardMarkedTextSelector = mkSelector "discardMarkedText"

-- | @Selector@ for @invalidateCharacterCoordinates@
invalidateCharacterCoordinatesSelector :: Selector '[] ()
invalidateCharacterCoordinatesSelector = mkSelector "invalidateCharacterCoordinates"

-- | @Selector@ for @textInputClientWillStartScrollingOrZooming@
textInputClientWillStartScrollingOrZoomingSelector :: Selector '[] ()
textInputClientWillStartScrollingOrZoomingSelector = mkSelector "textInputClientWillStartScrollingOrZooming"

-- | @Selector@ for @textInputClientDidEndScrollingOrZooming@
textInputClientDidEndScrollingOrZoomingSelector :: Selector '[] ()
textInputClientDidEndScrollingOrZoomingSelector = mkSelector "textInputClientDidEndScrollingOrZooming"

-- | @Selector@ for @textInputClientDidUpdateSelection@
textInputClientDidUpdateSelectionSelector :: Selector '[] ()
textInputClientDidUpdateSelectionSelector = mkSelector "textInputClientDidUpdateSelection"

-- | @Selector@ for @textInputClientDidScroll@
textInputClientDidScrollSelector :: Selector '[] ()
textInputClientDidScrollSelector = mkSelector "textInputClientDidScroll"

-- | @Selector@ for @localizedNameForInputSource:@
localizedNameForInputSourceSelector :: Selector '[Id NSString] (Id NSString)
localizedNameForInputSourceSelector = mkSelector "localizedNameForInputSource:"

-- | @Selector@ for @currentInputContext@
currentInputContextSelector :: Selector '[] (Id NSTextInputContext)
currentInputContextSelector = mkSelector "currentInputContext"

-- | @Selector@ for @client@
clientSelector :: Selector '[] RawId
clientSelector = mkSelector "client"

-- | @Selector@ for @acceptsGlyphInfo@
acceptsGlyphInfoSelector :: Selector '[] Bool
acceptsGlyphInfoSelector = mkSelector "acceptsGlyphInfo"

-- | @Selector@ for @setAcceptsGlyphInfo:@
setAcceptsGlyphInfoSelector :: Selector '[Bool] ()
setAcceptsGlyphInfoSelector = mkSelector "setAcceptsGlyphInfo:"

-- | @Selector@ for @allowedInputSourceLocales@
allowedInputSourceLocalesSelector :: Selector '[] (Id NSArray)
allowedInputSourceLocalesSelector = mkSelector "allowedInputSourceLocales"

-- | @Selector@ for @setAllowedInputSourceLocales:@
setAllowedInputSourceLocalesSelector :: Selector '[Id NSArray] ()
setAllowedInputSourceLocalesSelector = mkSelector "setAllowedInputSourceLocales:"

-- | @Selector@ for @keyboardInputSources@
keyboardInputSourcesSelector :: Selector '[] (Id NSArray)
keyboardInputSourcesSelector = mkSelector "keyboardInputSources"

-- | @Selector@ for @selectedKeyboardInputSource@
selectedKeyboardInputSourceSelector :: Selector '[] (Id NSString)
selectedKeyboardInputSourceSelector = mkSelector "selectedKeyboardInputSource"

-- | @Selector@ for @setSelectedKeyboardInputSource:@
setSelectedKeyboardInputSourceSelector :: Selector '[Id NSString] ()
setSelectedKeyboardInputSourceSelector = mkSelector "setSelectedKeyboardInputSource:"

