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
  , initSelector
  , newSelector
  , identifierSelector
  , invocationPhraseSelector
  , shortcutSelector


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

-- | An @INVoiceShortcut@ cannot be created directly. Instead, create an @INShortcut,@ and add it using @INUIAddVoiceShortcutViewController.@
--
-- ObjC selector: @- init@
init_ :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id INVoiceShortcut)
init_ inVoiceShortcut  =
  sendMsg inVoiceShortcut (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An @INVoiceShortcut@ cannot be created directly. Instead, create an @INShortcut,@ and add it using @INUIAddVoiceShortcutViewController.@
--
-- ObjC selector: @+ new@
new :: IO (Id INVoiceShortcut)
new  =
  do
    cls' <- getRequiredClass "INVoiceShortcut"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The unique identifier for this voice shortcut
--
-- ObjC selector: @- identifier@
identifier :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id NSUUID)
identifier inVoiceShortcut  =
  sendMsg inVoiceShortcut (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The phrase the user speaks to invoke this shortcut; set by the user when they add it to Siri.
--
-- ObjC selector: @- invocationPhrase@
invocationPhrase :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id NSString)
invocationPhrase inVoiceShortcut  =
  sendMsg inVoiceShortcut (mkSelector "invocationPhrase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The shortcut that will be performed when this voice shortcut is invoked via Siri.
--
-- ObjC selector: @- shortcut@
shortcut :: IsINVoiceShortcut inVoiceShortcut => inVoiceShortcut -> IO (Id INShortcut)
shortcut inVoiceShortcut  =
  sendMsg inVoiceShortcut (mkSelector "shortcut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @invocationPhrase@
invocationPhraseSelector :: Selector
invocationPhraseSelector = mkSelector "invocationPhrase"

-- | @Selector@ for @shortcut@
shortcutSelector :: Selector
shortcutSelector = mkSelector "shortcut"

