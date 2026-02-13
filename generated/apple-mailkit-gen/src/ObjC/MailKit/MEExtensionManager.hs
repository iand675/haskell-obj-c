{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Methods in this class allow the host app to interact with their Mail extension.
--
-- Generated bindings for @MEExtensionManager@.
module ObjC.MailKit.MEExtensionManager
  ( MEExtensionManager
  , IsMEExtensionManager(..)
  , new
  , init_
  , reloadContentBlockerWithIdentifier_completionHandler
  , reloadVisibleMessagesWithCompletionHandler
  , initSelector
  , newSelector
  , reloadContentBlockerWithIdentifier_completionHandlerSelector
  , reloadVisibleMessagesWithCompletionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEExtensionManager)
new  =
  do
    cls' <- getRequiredClass "MEExtensionManager"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEExtensionManager meExtensionManager => meExtensionManager -> IO (Id MEExtensionManager)
init_ meExtensionManager =
  sendOwnedMessage meExtensionManager initSelector

-- | This will call on Mail to reload the content rule list associated with the given identifier. Mail May throttle reloading the content blocker to once every few minutes.
--
-- ObjC selector: @+ reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
reloadContentBlockerWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "MEExtensionManager"
    sendClassMessage cls' reloadContentBlockerWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | This will call on Mail to reload the currently visible messages.  Mail may throttle reloading visible messages.
--
-- ObjC selector: @+ reloadVisibleMessagesWithCompletionHandler:@
reloadVisibleMessagesWithCompletionHandler :: Ptr () -> IO ()
reloadVisibleMessagesWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "MEExtensionManager"
    sendClassMessage cls' reloadVisibleMessagesWithCompletionHandlerSelector completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEExtensionManager)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEExtensionManager)
initSelector = mkSelector "init"

-- | @Selector@ for @reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
reloadContentBlockerWithIdentifier_completionHandlerSelector = mkSelector "reloadContentBlockerWithIdentifier:completionHandler:"

-- | @Selector@ for @reloadVisibleMessagesWithCompletionHandler:@
reloadVisibleMessagesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
reloadVisibleMessagesWithCompletionHandlerSelector = mkSelector "reloadVisibleMessagesWithCompletionHandler:"

