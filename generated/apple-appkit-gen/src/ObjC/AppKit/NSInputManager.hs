{-# LANGUAGE DataKinds #-}
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
  , handleMouseEventSelector
  , imageSelector
  , initWithName_hostSelector
  , languageSelector
  , localizedInputManagerNameSelector
  , markedTextAbandonedSelector
  , markedTextSelectionChanged_clientSelector
  , serverSelector
  , wantsToDelayTextChangeNotificationsSelector
  , wantsToHandleMouseEventsSelector
  , wantsToInterpretAllKeystrokesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' currentInputManagerSelector

-- | @+ cycleToNextInputLanguage:@
cycleToNextInputLanguage :: RawId -> IO ()
cycleToNextInputLanguage sender =
  do
    cls' <- getRequiredClass "NSInputManager"
    sendClassMessage cls' cycleToNextInputLanguageSelector sender

-- | @+ cycleToNextInputServerInLanguage:@
cycleToNextInputServerInLanguage :: RawId -> IO ()
cycleToNextInputServerInLanguage sender =
  do
    cls' <- getRequiredClass "NSInputManager"
    sendClassMessage cls' cycleToNextInputServerInLanguageSelector sender

-- | @- initWithName:host:@
initWithName_host :: (IsNSInputManager nsInputManager, IsNSString inputServerName, IsNSString hostName) => nsInputManager -> inputServerName -> hostName -> IO (Id NSInputManager)
initWithName_host nsInputManager inputServerName hostName =
  sendOwnedMessage nsInputManager initWithName_hostSelector (toNSString inputServerName) (toNSString hostName)

-- | @- localizedInputManagerName@
localizedInputManagerName :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSString)
localizedInputManagerName nsInputManager =
  sendMessage nsInputManager localizedInputManagerNameSelector

-- | @- markedTextAbandoned:@
markedTextAbandoned :: IsNSInputManager nsInputManager => nsInputManager -> RawId -> IO ()
markedTextAbandoned nsInputManager cli =
  sendMessage nsInputManager markedTextAbandonedSelector cli

-- | @- markedTextSelectionChanged:client:@
markedTextSelectionChanged_client :: IsNSInputManager nsInputManager => nsInputManager -> NSRange -> RawId -> IO ()
markedTextSelectionChanged_client nsInputManager newSel cli =
  sendMessage nsInputManager markedTextSelectionChanged_clientSelector newSel cli

-- | @- wantsToInterpretAllKeystrokes@
wantsToInterpretAllKeystrokes :: IsNSInputManager nsInputManager => nsInputManager -> IO Bool
wantsToInterpretAllKeystrokes nsInputManager =
  sendMessage nsInputManager wantsToInterpretAllKeystrokesSelector

-- | @- language@
language :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSString)
language nsInputManager =
  sendMessage nsInputManager languageSelector

-- | @- image@
image :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSImage)
image nsInputManager =
  sendMessage nsInputManager imageSelector

-- | @- server@
server :: IsNSInputManager nsInputManager => nsInputManager -> IO (Id NSInputServer)
server nsInputManager =
  sendMessage nsInputManager serverSelector

-- | @- wantsToHandleMouseEvents@
wantsToHandleMouseEvents :: IsNSInputManager nsInputManager => nsInputManager -> IO Bool
wantsToHandleMouseEvents nsInputManager =
  sendMessage nsInputManager wantsToHandleMouseEventsSelector

-- | @- handleMouseEvent:@
handleMouseEvent :: (IsNSInputManager nsInputManager, IsNSEvent mouseEvent) => nsInputManager -> mouseEvent -> IO Bool
handleMouseEvent nsInputManager mouseEvent =
  sendMessage nsInputManager handleMouseEventSelector (toNSEvent mouseEvent)

-- | @- wantsToDelayTextChangeNotifications@
wantsToDelayTextChangeNotifications :: IsNSInputManager nsInputManager => nsInputManager -> IO Bool
wantsToDelayTextChangeNotifications nsInputManager =
  sendMessage nsInputManager wantsToDelayTextChangeNotificationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentInputManager@
currentInputManagerSelector :: Selector '[] (Id NSInputManager)
currentInputManagerSelector = mkSelector "currentInputManager"

-- | @Selector@ for @cycleToNextInputLanguage:@
cycleToNextInputLanguageSelector :: Selector '[RawId] ()
cycleToNextInputLanguageSelector = mkSelector "cycleToNextInputLanguage:"

-- | @Selector@ for @cycleToNextInputServerInLanguage:@
cycleToNextInputServerInLanguageSelector :: Selector '[RawId] ()
cycleToNextInputServerInLanguageSelector = mkSelector "cycleToNextInputServerInLanguage:"

-- | @Selector@ for @initWithName:host:@
initWithName_hostSelector :: Selector '[Id NSString, Id NSString] (Id NSInputManager)
initWithName_hostSelector = mkSelector "initWithName:host:"

-- | @Selector@ for @localizedInputManagerName@
localizedInputManagerNameSelector :: Selector '[] (Id NSString)
localizedInputManagerNameSelector = mkSelector "localizedInputManagerName"

-- | @Selector@ for @markedTextAbandoned:@
markedTextAbandonedSelector :: Selector '[RawId] ()
markedTextAbandonedSelector = mkSelector "markedTextAbandoned:"

-- | @Selector@ for @markedTextSelectionChanged:client:@
markedTextSelectionChanged_clientSelector :: Selector '[NSRange, RawId] ()
markedTextSelectionChanged_clientSelector = mkSelector "markedTextSelectionChanged:client:"

-- | @Selector@ for @wantsToInterpretAllKeystrokes@
wantsToInterpretAllKeystrokesSelector :: Selector '[] Bool
wantsToInterpretAllKeystrokesSelector = mkSelector "wantsToInterpretAllKeystrokes"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @server@
serverSelector :: Selector '[] (Id NSInputServer)
serverSelector = mkSelector "server"

-- | @Selector@ for @wantsToHandleMouseEvents@
wantsToHandleMouseEventsSelector :: Selector '[] Bool
wantsToHandleMouseEventsSelector = mkSelector "wantsToHandleMouseEvents"

-- | @Selector@ for @handleMouseEvent:@
handleMouseEventSelector :: Selector '[Id NSEvent] Bool
handleMouseEventSelector = mkSelector "handleMouseEvent:"

-- | @Selector@ for @wantsToDelayTextChangeNotifications@
wantsToDelayTextChangeNotificationsSelector :: Selector '[] Bool
wantsToDelayTextChangeNotificationsSelector = mkSelector "wantsToDelayTextChangeNotifications"

