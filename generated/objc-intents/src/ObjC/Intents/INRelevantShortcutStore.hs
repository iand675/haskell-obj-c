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
  , setRelevantShortcuts_completionHandlerSelector
  , initSelector
  , defaultStoreSelector


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

-- | Provide a new set of relevant shortcuts that should be suggested by Siri.
--
-- Note: Setting new relevant shortcuts will replace all relevant shortcuts that were previously provided.
--
-- ObjC selector: @- setRelevantShortcuts:completionHandler:@
setRelevantShortcuts_completionHandler :: (IsINRelevantShortcutStore inRelevantShortcutStore, IsNSArray shortcuts) => inRelevantShortcutStore -> shortcuts -> Ptr () -> IO ()
setRelevantShortcuts_completionHandler inRelevantShortcutStore  shortcuts completionHandler =
withObjCPtr shortcuts $ \raw_shortcuts ->
    sendMsg inRelevantShortcutStore (mkSelector "setRelevantShortcuts:completionHandler:") retVoid [argPtr (castPtr raw_shortcuts :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Note: Use the @defaultStore@ singleton.
--
-- ObjC selector: @- init@
init_ :: IsINRelevantShortcutStore inRelevantShortcutStore => inRelevantShortcutStore -> IO (Id INRelevantShortcutStore)
init_ inRelevantShortcutStore  =
  sendMsg inRelevantShortcutStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ defaultStore@
defaultStore :: IO (Id INRelevantShortcutStore)
defaultStore  =
  do
    cls' <- getRequiredClass "INRelevantShortcutStore"
    sendClassMsg cls' (mkSelector "defaultStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setRelevantShortcuts:completionHandler:@
setRelevantShortcuts_completionHandlerSelector :: Selector
setRelevantShortcuts_completionHandlerSelector = mkSelector "setRelevantShortcuts:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @defaultStore@
defaultStoreSelector :: Selector
defaultStoreSelector = mkSelector "defaultStore"

