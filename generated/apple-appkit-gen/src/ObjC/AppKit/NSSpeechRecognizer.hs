{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSpeechRecognizer@.
module ObjC.AppKit.NSSpeechRecognizer
  ( NSSpeechRecognizer
  , IsNSSpeechRecognizer(..)
  , init_
  , startListening
  , stopListening
  , delegate
  , setDelegate
  , commands
  , setCommands
  , displayedCommandsTitle
  , setDisplayedCommandsTitle
  , listensInForegroundOnly
  , setListensInForegroundOnly
  , blocksOtherRecognizers
  , setBlocksOtherRecognizers
  , blocksOtherRecognizersSelector
  , commandsSelector
  , delegateSelector
  , displayedCommandsTitleSelector
  , initSelector
  , listensInForegroundOnlySelector
  , setBlocksOtherRecognizersSelector
  , setCommandsSelector
  , setDelegateSelector
  , setDisplayedCommandsTitleSelector
  , setListensInForegroundOnlySelector
  , startListeningSelector
  , stopListeningSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO (Id NSSpeechRecognizer)
init_ nsSpeechRecognizer =
  sendOwnedMessage nsSpeechRecognizer initSelector

-- | @- startListening@
startListening :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO ()
startListening nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer startListeningSelector

-- | @- stopListening@
stopListening :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO ()
stopListening nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer stopListeningSelector

-- | @- delegate@
delegate :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO RawId
delegate nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> RawId -> IO ()
setDelegate nsSpeechRecognizer value =
  sendMessage nsSpeechRecognizer setDelegateSelector value

-- | @- commands@
commands :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO (Id NSArray)
commands nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer commandsSelector

-- | @- setCommands:@
setCommands :: (IsNSSpeechRecognizer nsSpeechRecognizer, IsNSArray value) => nsSpeechRecognizer -> value -> IO ()
setCommands nsSpeechRecognizer value =
  sendMessage nsSpeechRecognizer setCommandsSelector (toNSArray value)

-- | @- displayedCommandsTitle@
displayedCommandsTitle :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO (Id NSString)
displayedCommandsTitle nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer displayedCommandsTitleSelector

-- | @- setDisplayedCommandsTitle:@
setDisplayedCommandsTitle :: (IsNSSpeechRecognizer nsSpeechRecognizer, IsNSString value) => nsSpeechRecognizer -> value -> IO ()
setDisplayedCommandsTitle nsSpeechRecognizer value =
  sendMessage nsSpeechRecognizer setDisplayedCommandsTitleSelector (toNSString value)

-- | @- listensInForegroundOnly@
listensInForegroundOnly :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO Bool
listensInForegroundOnly nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer listensInForegroundOnlySelector

-- | @- setListensInForegroundOnly:@
setListensInForegroundOnly :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> Bool -> IO ()
setListensInForegroundOnly nsSpeechRecognizer value =
  sendMessage nsSpeechRecognizer setListensInForegroundOnlySelector value

-- | @- blocksOtherRecognizers@
blocksOtherRecognizers :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO Bool
blocksOtherRecognizers nsSpeechRecognizer =
  sendMessage nsSpeechRecognizer blocksOtherRecognizersSelector

-- | @- setBlocksOtherRecognizers:@
setBlocksOtherRecognizers :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> Bool -> IO ()
setBlocksOtherRecognizers nsSpeechRecognizer value =
  sendMessage nsSpeechRecognizer setBlocksOtherRecognizersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSpeechRecognizer)
initSelector = mkSelector "init"

-- | @Selector@ for @startListening@
startListeningSelector :: Selector '[] ()
startListeningSelector = mkSelector "startListening"

-- | @Selector@ for @stopListening@
stopListeningSelector :: Selector '[] ()
stopListeningSelector = mkSelector "stopListening"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @commands@
commandsSelector :: Selector '[] (Id NSArray)
commandsSelector = mkSelector "commands"

-- | @Selector@ for @setCommands:@
setCommandsSelector :: Selector '[Id NSArray] ()
setCommandsSelector = mkSelector "setCommands:"

-- | @Selector@ for @displayedCommandsTitle@
displayedCommandsTitleSelector :: Selector '[] (Id NSString)
displayedCommandsTitleSelector = mkSelector "displayedCommandsTitle"

-- | @Selector@ for @setDisplayedCommandsTitle:@
setDisplayedCommandsTitleSelector :: Selector '[Id NSString] ()
setDisplayedCommandsTitleSelector = mkSelector "setDisplayedCommandsTitle:"

-- | @Selector@ for @listensInForegroundOnly@
listensInForegroundOnlySelector :: Selector '[] Bool
listensInForegroundOnlySelector = mkSelector "listensInForegroundOnly"

-- | @Selector@ for @setListensInForegroundOnly:@
setListensInForegroundOnlySelector :: Selector '[Bool] ()
setListensInForegroundOnlySelector = mkSelector "setListensInForegroundOnly:"

-- | @Selector@ for @blocksOtherRecognizers@
blocksOtherRecognizersSelector :: Selector '[] Bool
blocksOtherRecognizersSelector = mkSelector "blocksOtherRecognizers"

-- | @Selector@ for @setBlocksOtherRecognizers:@
setBlocksOtherRecognizersSelector :: Selector '[Bool] ()
setBlocksOtherRecognizersSelector = mkSelector "setBlocksOtherRecognizers:"

