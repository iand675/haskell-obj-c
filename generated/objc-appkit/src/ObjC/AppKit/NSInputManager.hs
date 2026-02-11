{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInputManager@.
module ObjC.AppKit.NSInputManager
  ( NSInputManager
  , IsNSInputManager(..)
  , currentInputManager
  , cycleToNextInputLanguage
  , cycleToNextInputServerInLanguage
  , initWithName_host
  , localizedInputManagerName
  , markedTextAbandoned
  , markedTextSelectionChanged_client
  , wantsToInterpretAllKeystrokes
  , language
  , image
  , server
  , wantsToHandleMouseEvents
  , handleMouseEvent
  , wantsToDelayTextChangeNotifications
  , currentInputManagerSelector
  , cycleToNextInputLanguageSelector
  , cycleToNextInputServerInLanguageSelector
  , initWithName_hostSelector
  , localizedInputManagerNameSelector
  , markedTextAbandonedSelector
  , markedTextSelectionChanged_clientSelector
  , wantsToInterpretAllKeystrokesSelector
  , languageSelector
  , imageSelector
  , serverSelector
  , wantsToHandleMouseEventsSelector
  , handleMouseEventSelector
  , wantsToDelayTextChangeNotificationsSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ currentInputManager@
currentInputManager :: IO (Id NSInputManager)
currentInputManager  =
  do
    cls' <- getRequiredClass "NSInputManager"
    sendClassMsg cls' (mkSelector "currentInputManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cycleToNextInputLanguage:@
cycleToNextInputLanguage :: RawId -> IO ()
cycleToNextInputLanguage sender =
  do
    cls' <- getRequiredClass "NSInputManager"
    sendClassMsg cls' (mkSelector "cycleToNextInputLanguage:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @+ cycleToNextInputServerInLanguage:@
cycleToNextInputServerInLanguage :: RawId -> IO ()
cycleToNextInputServerInLanguage sender =
  do
    cls' <- getRequiredClass "NSInputManager"
    sendClassMsg cls' (mkSelector "cycleToNextInputServerInLanguage:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- initWithName:host:@
initWithName_host :: (IsNSInputManager nsInputManager, IsNSString inputServerName, IsNSString hostName) => nsInputManager -> inputServerName -> hostName -> IO (Id NSInputManager)
initWithName_host nsInputManager  inputServerName hostName =
withObjCPtr inputServerName $ \raw_inputServerName ->
  withObjCPtr hostName $ \raw_hostName ->
      sendMsg nsInputManager (mkSelector "initWithName:host:") (retPtr retVoid) [argPtr (castPtr raw_inputServerName :: Ptr ()), argPtr (castPtr raw_hostName :: Ptr ())] >>= ownedObject . castPtr

-- | @- localizedInputManagerName@
localizedInputManagerName :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSString)
localizedInputManagerName nsInputManager  =
  sendMsg nsInputManager (mkSelector "localizedInputManagerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- markedTextAbandoned:@
markedTextAbandoned :: IsNSInputManager nsInputManager => nsInputManager -> RawId -> IO ()
markedTextAbandoned nsInputManager  cli =
  sendMsg nsInputManager (mkSelector "markedTextAbandoned:") retVoid [argPtr (castPtr (unRawId cli) :: Ptr ())]

-- | @- markedTextSelectionChanged:client:@
markedTextSelectionChanged_client :: IsNSInputManager nsInputManager => nsInputManager -> NSRange -> RawId -> IO ()
markedTextSelectionChanged_client nsInputManager  newSel cli =
  sendMsg nsInputManager (mkSelector "markedTextSelectionChanged:client:") retVoid [argNSRange newSel, argPtr (castPtr (unRawId cli) :: Ptr ())]

-- | @- wantsToInterpretAllKeystrokes@
wantsToInterpretAllKeystrokes :: IsNSInputManager nsInputManager => nsInputManager -> IO Bool
wantsToInterpretAllKeystrokes nsInputManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInputManager (mkSelector "wantsToInterpretAllKeystrokes") retCULong []

-- | @- language@
language :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSString)
language nsInputManager  =
  sendMsg nsInputManager (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- image@
image :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSImage)
image nsInputManager  =
  sendMsg nsInputManager (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- server@
server :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSInputServer)
server nsInputManager  =
  sendMsg nsInputManager (mkSelector "server") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- wantsToHandleMouseEvents@
wantsToHandleMouseEvents :: IsNSInputManager nsInputManager => nsInputManager -> IO Bool
wantsToHandleMouseEvents nsInputManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInputManager (mkSelector "wantsToHandleMouseEvents") retCULong []

-- | @- handleMouseEvent:@
handleMouseEvent :: (IsNSInputManager nsInputManager, IsNSEvent mouseEvent) => nsInputManager -> mouseEvent -> IO Bool
handleMouseEvent nsInputManager  mouseEvent =
withObjCPtr mouseEvent $ \raw_mouseEvent ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInputManager (mkSelector "handleMouseEvent:") retCULong [argPtr (castPtr raw_mouseEvent :: Ptr ())]

-- | @- wantsToDelayTextChangeNotifications@
wantsToDelayTextChangeNotifications :: IsNSInputManager nsInputManager => nsInputManager -> IO Bool
wantsToDelayTextChangeNotifications nsInputManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInputManager (mkSelector "wantsToDelayTextChangeNotifications") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentInputManager@
currentInputManagerSelector :: Selector
currentInputManagerSelector = mkSelector "currentInputManager"

-- | @Selector@ for @cycleToNextInputLanguage:@
cycleToNextInputLanguageSelector :: Selector
cycleToNextInputLanguageSelector = mkSelector "cycleToNextInputLanguage:"

-- | @Selector@ for @cycleToNextInputServerInLanguage:@
cycleToNextInputServerInLanguageSelector :: Selector
cycleToNextInputServerInLanguageSelector = mkSelector "cycleToNextInputServerInLanguage:"

-- | @Selector@ for @initWithName:host:@
initWithName_hostSelector :: Selector
initWithName_hostSelector = mkSelector "initWithName:host:"

-- | @Selector@ for @localizedInputManagerName@
localizedInputManagerNameSelector :: Selector
localizedInputManagerNameSelector = mkSelector "localizedInputManagerName"

-- | @Selector@ for @markedTextAbandoned:@
markedTextAbandonedSelector :: Selector
markedTextAbandonedSelector = mkSelector "markedTextAbandoned:"

-- | @Selector@ for @markedTextSelectionChanged:client:@
markedTextSelectionChanged_clientSelector :: Selector
markedTextSelectionChanged_clientSelector = mkSelector "markedTextSelectionChanged:client:"

-- | @Selector@ for @wantsToInterpretAllKeystrokes@
wantsToInterpretAllKeystrokesSelector :: Selector
wantsToInterpretAllKeystrokesSelector = mkSelector "wantsToInterpretAllKeystrokes"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @server@
serverSelector :: Selector
serverSelector = mkSelector "server"

-- | @Selector@ for @wantsToHandleMouseEvents@
wantsToHandleMouseEventsSelector :: Selector
wantsToHandleMouseEventsSelector = mkSelector "wantsToHandleMouseEvents"

-- | @Selector@ for @handleMouseEvent:@
handleMouseEventSelector :: Selector
handleMouseEventSelector = mkSelector "handleMouseEvent:"

-- | @Selector@ for @wantsToDelayTextChangeNotifications@
wantsToDelayTextChangeNotificationsSelector :: Selector
wantsToDelayTextChangeNotificationsSelector = mkSelector "wantsToDelayTextChangeNotifications"

