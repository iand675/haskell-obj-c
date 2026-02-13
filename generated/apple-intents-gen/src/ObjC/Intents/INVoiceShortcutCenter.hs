{-# LANGUAGE DataKinds #-}
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
  , getVoiceShortcutWithIdentifier_completionSelector
  , initSelector
  , newSelector
  , setShortcutSuggestionsSelector
  , sharedCenterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendOwnedClassMessage cls' newSelector

-- | Note: Use the @sharedCenter@ singleton.
--
-- ObjC selector: @- init@
init_ :: IsINVoiceShortcutCenter inVoiceShortcutCenter => inVoiceShortcutCenter -> IO (Id INVoiceShortcutCenter)
init_ inVoiceShortcutCenter =
  sendOwnedMessage inVoiceShortcutCenter initSelector

-- | Get a single shortcut (associated with this app) that has been added to Siri, by its identifier.
--
-- ObjC selector: @- getVoiceShortcutWithIdentifier:completion:@
getVoiceShortcutWithIdentifier_completion :: (IsINVoiceShortcutCenter inVoiceShortcutCenter, IsNSUUID identifier) => inVoiceShortcutCenter -> identifier -> Ptr () -> IO ()
getVoiceShortcutWithIdentifier_completion inVoiceShortcutCenter identifier completionHandler =
  sendMessage inVoiceShortcutCenter getVoiceShortcutWithIdentifier_completionSelector (toNSUUID identifier) completionHandler

-- | Set some shortcuts that should be suggested to the user to add to Siri.
--
-- These suggestions are shown to the user in the Shortcuts app.
--
-- ObjC selector: @- setShortcutSuggestions:@
setShortcutSuggestions :: (IsINVoiceShortcutCenter inVoiceShortcutCenter, IsNSArray suggestions) => inVoiceShortcutCenter -> suggestions -> IO ()
setShortcutSuggestions inVoiceShortcutCenter suggestions =
  sendMessage inVoiceShortcutCenter setShortcutSuggestionsSelector (toNSArray suggestions)

-- | @+ sharedCenter@
sharedCenter :: IO (Id INVoiceShortcutCenter)
sharedCenter  =
  do
    cls' <- getRequiredClass "INVoiceShortcutCenter"
    sendClassMessage cls' sharedCenterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id INVoiceShortcutCenter)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INVoiceShortcutCenter)
initSelector = mkSelector "init"

-- | @Selector@ for @getVoiceShortcutWithIdentifier:completion:@
getVoiceShortcutWithIdentifier_completionSelector :: Selector '[Id NSUUID, Ptr ()] ()
getVoiceShortcutWithIdentifier_completionSelector = mkSelector "getVoiceShortcutWithIdentifier:completion:"

-- | @Selector@ for @setShortcutSuggestions:@
setShortcutSuggestionsSelector :: Selector '[Id NSArray] ()
setShortcutSuggestionsSelector = mkSelector "setShortcutSuggestions:"

-- | @Selector@ for @sharedCenter@
sharedCenterSelector :: Selector '[] (Id INVoiceShortcutCenter)
sharedCenterSelector = mkSelector "sharedCenter"

