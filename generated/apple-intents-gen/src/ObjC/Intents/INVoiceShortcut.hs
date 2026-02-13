{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A shortcut that has been added to Siri
--
-- Generated bindings for @INVoiceShortcut@.
module ObjC.Intents.INVoiceShortcut
  ( INVoiceShortcut
  , IsINVoiceShortcut(..)
  , init_
  , new
  , identifier
  , invocationPhrase
  , shortcut
  , identifierSelector
  , initSelector
  , invocationPhraseSelector
  , newSelector
  , shortcutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An @INVoiceShortcut@ cannot be created directly. Instead, create an @INShortcut,@ and add it using @INUIAddVoiceShortcutViewController.@
--
-- ObjC selector: @- init@
init_ :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id INVoiceShortcut)
init_ inVoiceShortcut =
  sendOwnedMessage inVoiceShortcut initSelector

-- | An @INVoiceShortcut@ cannot be created directly. Instead, create an @INShortcut,@ and add it using @INUIAddVoiceShortcutViewController.@
--
-- ObjC selector: @+ new@
new :: IO (Id INVoiceShortcut)
new  =
  do
    cls' <- getRequiredClass "INVoiceShortcut"
    sendOwnedClassMessage cls' newSelector

-- | The unique identifier for this voice shortcut
--
-- ObjC selector: @- identifier@
identifier :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id NSUUID)
identifier inVoiceShortcut =
  sendMessage inVoiceShortcut identifierSelector

-- | The phrase the user speaks to invoke this shortcut; set by the user when they add it to Siri.
--
-- ObjC selector: @- invocationPhrase@
invocationPhrase :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id NSString)
invocationPhrase inVoiceShortcut =
  sendMessage inVoiceShortcut invocationPhraseSelector

-- | The shortcut that will be performed when this voice shortcut is invoked via Siri.
--
-- ObjC selector: @- shortcut@
shortcut :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id INShortcut)
shortcut inVoiceShortcut =
  sendMessage inVoiceShortcut shortcutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INVoiceShortcut)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id INVoiceShortcut)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @invocationPhrase@
invocationPhraseSelector :: Selector '[] (Id NSString)
invocationPhraseSelector = mkSelector "invocationPhrase"

-- | @Selector@ for @shortcut@
shortcutSelector :: Selector '[] (Id INShortcut)
shortcutSelector = mkSelector "shortcut"

