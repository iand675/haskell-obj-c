{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallDirectoryManager@.
module ObjC.CallKit.CXCallDirectoryManager
  ( CXCallDirectoryManager
  , IsCXCallDirectoryManager(..)
  , reloadExtensionWithIdentifier_completionHandler
  , getEnabledStatusForExtensionWithIdentifier_completionHandler
  , openSettingsWithCompletionHandler
  , sharedInstance
  , getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector
  , openSettingsWithCompletionHandlerSelector
  , reloadExtensionWithIdentifier_completionHandlerSelector
  , sharedInstanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reloadExtensionWithIdentifier:completionHandler:@
reloadExtensionWithIdentifier_completionHandler :: (IsCXCallDirectoryManager cxCallDirectoryManager, IsNSString identifier) => cxCallDirectoryManager -> identifier -> Ptr () -> IO ()
reloadExtensionWithIdentifier_completionHandler cxCallDirectoryManager identifier completion =
  sendMessage cxCallDirectoryManager reloadExtensionWithIdentifier_completionHandlerSelector (toNSString identifier) completion

-- | @- getEnabledStatusForExtensionWithIdentifier:completionHandler:@
getEnabledStatusForExtensionWithIdentifier_completionHandler :: (IsCXCallDirectoryManager cxCallDirectoryManager, IsNSString identifier) => cxCallDirectoryManager -> identifier -> Ptr () -> IO ()
getEnabledStatusForExtensionWithIdentifier_completionHandler cxCallDirectoryManager identifier completion =
  sendMessage cxCallDirectoryManager getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector (toNSString identifier) completion

-- | @- openSettingsWithCompletionHandler:@
openSettingsWithCompletionHandler :: IsCXCallDirectoryManager cxCallDirectoryManager => cxCallDirectoryManager -> Ptr () -> IO ()
openSettingsWithCompletionHandler cxCallDirectoryManager completion =
  sendMessage cxCallDirectoryManager openSettingsWithCompletionHandlerSelector completion

-- | @+ sharedInstance@
sharedInstance :: IO (Id CXCallDirectoryManager)
sharedInstance  =
  do
    cls' <- getRequiredClass "CXCallDirectoryManager"
    sendClassMessage cls' sharedInstanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadExtensionWithIdentifier:completionHandler:@
reloadExtensionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
reloadExtensionWithIdentifier_completionHandlerSelector = mkSelector "reloadExtensionWithIdentifier:completionHandler:"

-- | @Selector@ for @getEnabledStatusForExtensionWithIdentifier:completionHandler:@
getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector = mkSelector "getEnabledStatusForExtensionWithIdentifier:completionHandler:"

-- | @Selector@ for @openSettingsWithCompletionHandler:@
openSettingsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
openSettingsWithCompletionHandlerSelector = mkSelector "openSettingsWithCompletionHandler:"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id CXCallDirectoryManager)
sharedInstanceSelector = mkSelector "sharedInstance"

