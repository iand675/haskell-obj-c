{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lets you access shortcuts that have been added to Siri
--
-- INVoiceShortcut
--
-- Generated bindings for @INVoiceShortcutCenter@.
module ObjC.Intents.INVoiceShortcutCenter
  ( INVoiceShortcutCenter
  , IsINVoiceShortcutCenter(..)
  , new
  , init_
  , getVoiceShortcutWithIdentifier_completion
  , setShortcutSuggestions
  , sharedCenter
  , newSelector
  , initSelector
  , getVoiceShortcutWithIdentifier_completionSelector
  , setShortcutSuggestionsSelector
  , sharedCenterSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Note: Use the @sharedCenter@ singleton.
--
-- ObjC selector: @+ new@
new :: IO (Id INVoiceShortcutCenter)
new  =
  do
    cls' <- getRequiredClass "INVoiceShortcutCenter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Note: Use the @sharedCenter@ singleton.
--
-- ObjC selector: @- init@
init_ :: IsINVoiceShortcutCenter inVoiceShortcutCenter => inVoiceShortcutCenter -> IO (Id INVoiceShortcutCenter)
init_ inVoiceShortcutCenter  =
  sendMsg inVoiceShortcutCenter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get a single shortcut (associated with this app) that has been added to Siri, by its identifier.
--
-- ObjC selector: @- getVoiceShortcutWithIdentifier:completion:@
getVoiceShortcutWithIdentifier_completion :: (IsINVoiceShortcutCenter inVoiceShortcutCenter, IsNSUUID identifier) => inVoiceShortcutCenter -> identifier -> Ptr () -> IO ()
getVoiceShortcutWithIdentifier_completion inVoiceShortcutCenter  identifier completionHandler =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg inVoiceShortcutCenter (mkSelector "getVoiceShortcutWithIdentifier:completion:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Set some shortcuts that should be suggested to the user to add to Siri.
--
-- These suggestions are shown to the user in the Shortcuts app.
--
-- ObjC selector: @- setShortcutSuggestions:@
setShortcutSuggestions :: (IsINVoiceShortcutCenter inVoiceShortcutCenter, IsNSArray suggestions) => inVoiceShortcutCenter -> suggestions -> IO ()
setShortcutSuggestions inVoiceShortcutCenter  suggestions =
withObjCPtr suggestions $ \raw_suggestions ->
    sendMsg inVoiceShortcutCenter (mkSelector "setShortcutSuggestions:") retVoid [argPtr (castPtr raw_suggestions :: Ptr ())]

-- | @+ sharedCenter@
sharedCenter :: IO (Id INVoiceShortcutCenter)
sharedCenter  =
  do
    cls' <- getRequiredClass "INVoiceShortcutCenter"
    sendClassMsg cls' (mkSelector "sharedCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @getVoiceShortcutWithIdentifier:completion:@
getVoiceShortcutWithIdentifier_completionSelector :: Selector
getVoiceShortcutWithIdentifier_completionSelector = mkSelector "getVoiceShortcutWithIdentifier:completion:"

-- | @Selector@ for @setShortcutSuggestions:@
setShortcutSuggestionsSelector :: Selector
setShortcutSuggestionsSelector = mkSelector "setShortcutSuggestions:"

-- | @Selector@ for @sharedCenter@
sharedCenterSelector :: Selector
sharedCenterSelector = mkSelector "sharedCenter"

