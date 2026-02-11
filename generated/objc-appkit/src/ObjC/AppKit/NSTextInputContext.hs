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
  , acceptsGlyphInfo
  , setAcceptsGlyphInfo
  , allowedInputSourceLocales
  , setAllowedInputSourceLocales
  , keyboardInputSources
  , selectedKeyboardInputSource
  , setSelectedKeyboardInputSource
  , initWithClientSelector
  , initSelector
  , activateSelector
  , deactivateSelector
  , handleEventSelector
  , discardMarkedTextSelector
  , invalidateCharacterCoordinatesSelector
  , textInputClientWillStartScrollingOrZoomingSelector
  , textInputClientDidEndScrollingOrZoomingSelector
  , textInputClientDidUpdateSelectionSelector
  , textInputClientDidScrollSelector
  , localizedNameForInputSourceSelector
  , currentInputContextSelector
  , acceptsGlyphInfoSelector
  , setAcceptsGlyphInfoSelector
  , allowedInputSourceLocalesSelector
  , setAllowedInputSourceLocalesSelector
  , keyboardInputSourcesSelector
  , selectedKeyboardInputSourceSelector
  , setSelectedKeyboardInputSourceSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithClient:@
initWithClient :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> RawId -> IO (Id NSTextInputContext)
initWithClient nsTextInputContext  client =
  sendMsg nsTextInputContext (mkSelector "initWithClient:") (retPtr retVoid) [argPtr (castPtr (unRawId client) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSTextInputContext)
init_ nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | ** Activation ****
--
-- ObjC selector: @- activate@
activate :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
activate nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "activate") retVoid []

-- | @- deactivate@
deactivate :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
deactivate nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "deactivate") retVoid []

-- | ** Input source interface ***
--
-- ObjC selector: @- handleEvent:@
handleEvent :: (IsNSTextInputContext nsTextInputContext, IsNSEvent event) => nsTextInputContext -> event -> IO Bool
handleEvent nsTextInputContext  event =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextInputContext (mkSelector "handleEvent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- discardMarkedText@
discardMarkedText :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
discardMarkedText nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "discardMarkedText") retVoid []

-- | @- invalidateCharacterCoordinates@
invalidateCharacterCoordinates :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
invalidateCharacterCoordinates nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "invalidateCharacterCoordinates") retVoid []

-- | @- textInputClientWillStartScrollingOrZooming@
textInputClientWillStartScrollingOrZooming :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientWillStartScrollingOrZooming nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "textInputClientWillStartScrollingOrZooming") retVoid []

-- | @- textInputClientDidEndScrollingOrZooming@
textInputClientDidEndScrollingOrZooming :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientDidEndScrollingOrZooming nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "textInputClientDidEndScrollingOrZooming") retVoid []

-- | @- textInputClientDidUpdateSelection@
textInputClientDidUpdateSelection :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientDidUpdateSelection nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "textInputClientDidUpdateSelection") retVoid []

-- | @- textInputClientDidScroll@
textInputClientDidScroll :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO ()
textInputClientDidScroll nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "textInputClientDidScroll") retVoid []

-- | ** Text Input source attributes ***
--
-- ObjC selector: @+ localizedNameForInputSource:@
localizedNameForInputSource :: IsNSString inputSourceIdentifier => inputSourceIdentifier -> IO (Id NSString)
localizedNameForInputSource inputSourceIdentifier =
  do
    cls' <- getRequiredClass "NSTextInputContext"
    withObjCPtr inputSourceIdentifier $ \raw_inputSourceIdentifier ->
      sendClassMsg cls' (mkSelector "localizedNameForInputSource:") (retPtr retVoid) [argPtr (castPtr raw_inputSourceIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ currentInputContext@
currentInputContext :: IO (Id NSTextInputContext)
currentInputContext  =
  do
    cls' <- getRequiredClass "NSTextInputContext"
    sendClassMsg cls' (mkSelector "currentInputContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- acceptsGlyphInfo@
acceptsGlyphInfo :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO Bool
acceptsGlyphInfo nsTextInputContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextInputContext (mkSelector "acceptsGlyphInfo") retCULong []

-- | @- setAcceptsGlyphInfo:@
setAcceptsGlyphInfo :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> Bool -> IO ()
setAcceptsGlyphInfo nsTextInputContext  value =
  sendMsg nsTextInputContext (mkSelector "setAcceptsGlyphInfo:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowedInputSourceLocales@
allowedInputSourceLocales :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSArray)
allowedInputSourceLocales nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "allowedInputSourceLocales") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAllowedInputSourceLocales:@
setAllowedInputSourceLocales :: (IsNSTextInputContext nsTextInputContext, IsNSArray value) => nsTextInputContext -> value -> IO ()
setAllowedInputSourceLocales nsTextInputContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextInputContext (mkSelector "setAllowedInputSourceLocales:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | ** Text Input sources handling ***
--
-- ObjC selector: @- keyboardInputSources@
keyboardInputSources :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSArray)
keyboardInputSources nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "keyboardInputSources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedKeyboardInputSource@
selectedKeyboardInputSource :: IsNSTextInputContext nsTextInputContext => nsTextInputContext -> IO (Id NSString)
selectedKeyboardInputSource nsTextInputContext  =
  sendMsg nsTextInputContext (mkSelector "selectedKeyboardInputSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedKeyboardInputSource:@
setSelectedKeyboardInputSource :: (IsNSTextInputContext nsTextInputContext, IsNSString value) => nsTextInputContext -> value -> IO ()
setSelectedKeyboardInputSource nsTextInputContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextInputContext (mkSelector "setSelectedKeyboardInputSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithClient:@
initWithClientSelector :: Selector
initWithClientSelector = mkSelector "initWithClient:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @activate@
activateSelector :: Selector
activateSelector = mkSelector "activate"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @handleEvent:@
handleEventSelector :: Selector
handleEventSelector = mkSelector "handleEvent:"

-- | @Selector@ for @discardMarkedText@
discardMarkedTextSelector :: Selector
discardMarkedTextSelector = mkSelector "discardMarkedText"

-- | @Selector@ for @invalidateCharacterCoordinates@
invalidateCharacterCoordinatesSelector :: Selector
invalidateCharacterCoordinatesSelector = mkSelector "invalidateCharacterCoordinates"

-- | @Selector@ for @textInputClientWillStartScrollingOrZooming@
textInputClientWillStartScrollingOrZoomingSelector :: Selector
textInputClientWillStartScrollingOrZoomingSelector = mkSelector "textInputClientWillStartScrollingOrZooming"

-- | @Selector@ for @textInputClientDidEndScrollingOrZooming@
textInputClientDidEndScrollingOrZoomingSelector :: Selector
textInputClientDidEndScrollingOrZoomingSelector = mkSelector "textInputClientDidEndScrollingOrZooming"

-- | @Selector@ for @textInputClientDidUpdateSelection@
textInputClientDidUpdateSelectionSelector :: Selector
textInputClientDidUpdateSelectionSelector = mkSelector "textInputClientDidUpdateSelection"

-- | @Selector@ for @textInputClientDidScroll@
textInputClientDidScrollSelector :: Selector
textInputClientDidScrollSelector = mkSelector "textInputClientDidScroll"

-- | @Selector@ for @localizedNameForInputSource:@
localizedNameForInputSourceSelector :: Selector
localizedNameForInputSourceSelector = mkSelector "localizedNameForInputSource:"

-- | @Selector@ for @currentInputContext@
currentInputContextSelector :: Selector
currentInputContextSelector = mkSelector "currentInputContext"

-- | @Selector@ for @acceptsGlyphInfo@
acceptsGlyphInfoSelector :: Selector
acceptsGlyphInfoSelector = mkSelector "acceptsGlyphInfo"

-- | @Selector@ for @setAcceptsGlyphInfo:@
setAcceptsGlyphInfoSelector :: Selector
setAcceptsGlyphInfoSelector = mkSelector "setAcceptsGlyphInfo:"

-- | @Selector@ for @allowedInputSourceLocales@
allowedInputSourceLocalesSelector :: Selector
allowedInputSourceLocalesSelector = mkSelector "allowedInputSourceLocales"

-- | @Selector@ for @setAllowedInputSourceLocales:@
setAllowedInputSourceLocalesSelector :: Selector
setAllowedInputSourceLocalesSelector = mkSelector "setAllowedInputSourceLocales:"

-- | @Selector@ for @keyboardInputSources@
keyboardInputSourcesSelector :: Selector
keyboardInputSourcesSelector = mkSelector "keyboardInputSources"

-- | @Selector@ for @selectedKeyboardInputSource@
selectedKeyboardInputSourceSelector :: Selector
selectedKeyboardInputSourceSelector = mkSelector "selectedKeyboardInputSource"

-- | @Selector@ for @setSelectedKeyboardInputSource:@
setSelectedKeyboardInputSourceSelector :: Selector
setSelectedKeyboardInputSourceSelector = mkSelector "setSelectedKeyboardInputSource:"

