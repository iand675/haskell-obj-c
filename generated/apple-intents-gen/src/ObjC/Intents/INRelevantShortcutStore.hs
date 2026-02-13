{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Where relevant shortcuts are provided to Siri.
--
-- INRelevantShortcut
--
-- Generated bindings for @INRelevantShortcutStore@.
module ObjC.Intents.INRelevantShortcutStore
  ( INRelevantShortcutStore
  , IsINRelevantShortcutStore(..)
  , setRelevantShortcuts_completionHandler
  , init_
  , defaultStore
  , defaultStoreSelector
  , initSelector
  , setRelevantShortcuts_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provide a new set of relevant shortcuts that should be suggested by Siri.
--
-- Note: Setting new relevant shortcuts will replace all relevant shortcuts that were previously provided.
--
-- ObjC selector: @- setRelevantShortcuts:completionHandler:@
setRelevantShortcuts_completionHandler :: (IsINRelevantShortcutStore inRelevantShortcutStore, IsNSArray shortcuts) => inRelevantShortcutStore -> shortcuts -> Ptr () -> IO ()
setRelevantShortcuts_completionHandler inRelevantShortcutStore shortcuts completionHandler =
  sendMessage inRelevantShortcutStore setRelevantShortcuts_completionHandlerSelector (toNSArray shortcuts) completionHandler

-- | Note: Use the @defaultStore@ singleton.
--
-- ObjC selector: @- init@
init_ :: IsINRelevantShortcutStore inRelevantShortcutStore => inRelevantShortcutStore -> IO (Id INRelevantShortcutStore)
init_ inRelevantShortcutStore =
  sendOwnedMessage inRelevantShortcutStore initSelector

-- | @+ defaultStore@
defaultStore :: IO (Id INRelevantShortcutStore)
defaultStore  =
  do
    cls' <- getRequiredClass "INRelevantShortcutStore"
    sendClassMessage cls' defaultStoreSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setRelevantShortcuts:completionHandler:@
setRelevantShortcuts_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
setRelevantShortcuts_completionHandlerSelector = mkSelector "setRelevantShortcuts:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRelevantShortcutStore)
initSelector = mkSelector "init"

-- | @Selector@ for @defaultStore@
defaultStoreSelector :: Selector '[] (Id INRelevantShortcutStore)
defaultStoreSelector = mkSelector "defaultStore"

