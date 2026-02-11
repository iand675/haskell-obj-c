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
  , reloadExtensionWithIdentifier_completionHandlerSelector
  , getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector
  , openSettingsWithCompletionHandlerSelector
  , sharedInstanceSelector


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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reloadExtensionWithIdentifier:completionHandler:@
reloadExtensionWithIdentifier_completionHandler :: (IsCXCallDirectoryManager cxCallDirectoryManager, IsNSString identifier) => cxCallDirectoryManager -> identifier -> Ptr () -> IO ()
reloadExtensionWithIdentifier_completionHandler cxCallDirectoryManager  identifier completion =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg cxCallDirectoryManager (mkSelector "reloadExtensionWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getEnabledStatusForExtensionWithIdentifier:completionHandler:@
getEnabledStatusForExtensionWithIdentifier_completionHandler :: (IsCXCallDirectoryManager cxCallDirectoryManager, IsNSString identifier) => cxCallDirectoryManager -> identifier -> Ptr () -> IO ()
getEnabledStatusForExtensionWithIdentifier_completionHandler cxCallDirectoryManager  identifier completion =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg cxCallDirectoryManager (mkSelector "getEnabledStatusForExtensionWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- openSettingsWithCompletionHandler:@
openSettingsWithCompletionHandler :: IsCXCallDirectoryManager cxCallDirectoryManager => cxCallDirectoryManager -> Ptr () -> IO ()
openSettingsWithCompletionHandler cxCallDirectoryManager  completion =
  sendMsg cxCallDirectoryManager (mkSelector "openSettingsWithCompletionHandler:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @+ sharedInstance@
sharedInstance :: IO (Id CXCallDirectoryManager)
sharedInstance  =
  do
    cls' <- getRequiredClass "CXCallDirectoryManager"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadExtensionWithIdentifier:completionHandler:@
reloadExtensionWithIdentifier_completionHandlerSelector :: Selector
reloadExtensionWithIdentifier_completionHandlerSelector = mkSelector "reloadExtensionWithIdentifier:completionHandler:"

-- | @Selector@ for @getEnabledStatusForExtensionWithIdentifier:completionHandler:@
getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector :: Selector
getEnabledStatusForExtensionWithIdentifier_completionHandlerSelector = mkSelector "getEnabledStatusForExtensionWithIdentifier:completionHandler:"

-- | @Selector@ for @openSettingsWithCompletionHandler:@
openSettingsWithCompletionHandlerSelector :: Selector
openSettingsWithCompletionHandlerSelector = mkSelector "openSettingsWithCompletionHandler:"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

