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
  , commands
  , setCommands
  , displayedCommandsTitle
  , setDisplayedCommandsTitle
  , listensInForegroundOnly
  , setListensInForegroundOnly
  , blocksOtherRecognizers
  , setBlocksOtherRecognizers
  , initSelector
  , startListeningSelector
  , stopListeningSelector
  , commandsSelector
  , setCommandsSelector
  , displayedCommandsTitleSelector
  , setDisplayedCommandsTitleSelector
  , listensInForegroundOnlySelector
  , setListensInForegroundOnlySelector
  , blocksOtherRecognizersSelector
  , setBlocksOtherRecognizersSelector


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

-- | @- init@
init_ :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO (Id NSSpeechRecognizer)
init_ nsSpeechRecognizer  =
  sendMsg nsSpeechRecognizer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- startListening@
startListening :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO ()
startListening nsSpeechRecognizer  =
  sendMsg nsSpeechRecognizer (mkSelector "startListening") retVoid []

-- | @- stopListening@
stopListening :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO ()
stopListening nsSpeechRecognizer  =
  sendMsg nsSpeechRecognizer (mkSelector "stopListening") retVoid []

-- | @- commands@
commands :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO (Id NSArray)
commands nsSpeechRecognizer  =
  sendMsg nsSpeechRecognizer (mkSelector "commands") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCommands:@
setCommands :: (IsNSSpeechRecognizer nsSpeechRecognizer, IsNSArray value) => nsSpeechRecognizer -> value -> IO ()
setCommands nsSpeechRecognizer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSpeechRecognizer (mkSelector "setCommands:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displayedCommandsTitle@
displayedCommandsTitle :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO (Id NSString)
displayedCommandsTitle nsSpeechRecognizer  =
  sendMsg nsSpeechRecognizer (mkSelector "displayedCommandsTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayedCommandsTitle:@
setDisplayedCommandsTitle :: (IsNSSpeechRecognizer nsSpeechRecognizer, IsNSString value) => nsSpeechRecognizer -> value -> IO ()
setDisplayedCommandsTitle nsSpeechRecognizer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSpeechRecognizer (mkSelector "setDisplayedCommandsTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- listensInForegroundOnly@
listensInForegroundOnly :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO Bool
listensInForegroundOnly nsSpeechRecognizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechRecognizer (mkSelector "listensInForegroundOnly") retCULong []

-- | @- setListensInForegroundOnly:@
setListensInForegroundOnly :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> Bool -> IO ()
setListensInForegroundOnly nsSpeechRecognizer  value =
  sendMsg nsSpeechRecognizer (mkSelector "setListensInForegroundOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- blocksOtherRecognizers@
blocksOtherRecognizers :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> IO Bool
blocksOtherRecognizers nsSpeechRecognizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechRecognizer (mkSelector "blocksOtherRecognizers") retCULong []

-- | @- setBlocksOtherRecognizers:@
setBlocksOtherRecognizers :: IsNSSpeechRecognizer nsSpeechRecognizer => nsSpeechRecognizer -> Bool -> IO ()
setBlocksOtherRecognizers nsSpeechRecognizer  value =
  sendMsg nsSpeechRecognizer (mkSelector "setBlocksOtherRecognizers:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @startListening@
startListeningSelector :: Selector
startListeningSelector = mkSelector "startListening"

-- | @Selector@ for @stopListening@
stopListeningSelector :: Selector
stopListeningSelector = mkSelector "stopListening"

-- | @Selector@ for @commands@
commandsSelector :: Selector
commandsSelector = mkSelector "commands"

-- | @Selector@ for @setCommands:@
setCommandsSelector :: Selector
setCommandsSelector = mkSelector "setCommands:"

-- | @Selector@ for @displayedCommandsTitle@
displayedCommandsTitleSelector :: Selector
displayedCommandsTitleSelector = mkSelector "displayedCommandsTitle"

-- | @Selector@ for @setDisplayedCommandsTitle:@
setDisplayedCommandsTitleSelector :: Selector
setDisplayedCommandsTitleSelector = mkSelector "setDisplayedCommandsTitle:"

-- | @Selector@ for @listensInForegroundOnly@
listensInForegroundOnlySelector :: Selector
listensInForegroundOnlySelector = mkSelector "listensInForegroundOnly"

-- | @Selector@ for @setListensInForegroundOnly:@
setListensInForegroundOnlySelector :: Selector
setListensInForegroundOnlySelector = mkSelector "setListensInForegroundOnly:"

-- | @Selector@ for @blocksOtherRecognizers@
blocksOtherRecognizersSelector :: Selector
blocksOtherRecognizersSelector = mkSelector "blocksOtherRecognizers"

-- | @Selector@ for @setBlocksOtherRecognizers:@
setBlocksOtherRecognizersSelector :: Selector
setBlocksOtherRecognizersSelector = mkSelector "setBlocksOtherRecognizers:"

