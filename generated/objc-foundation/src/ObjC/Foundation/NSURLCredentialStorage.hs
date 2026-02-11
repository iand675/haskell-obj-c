{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLCredentialStorage
--
-- NSURLCredentialStorage implements a singleton object (shared instance) which manages the shared credentials cache. Note: Whereas in Mac OS X any application can access any credential with a persistence of NSURLCredentialPersistencePermanent provided the user gives permission, in iPhone OS an application can access only its own credentials.
--
-- Generated bindings for @NSURLCredentialStorage@.
module ObjC.Foundation.NSURLCredentialStorage
  ( NSURLCredentialStorage
  , IsNSURLCredentialStorage(..)
  , credentialsForProtectionSpace
  , setCredential_forProtectionSpace
  , removeCredential_forProtectionSpace
  , removeCredential_forProtectionSpace_options
  , defaultCredentialForProtectionSpace
  , setDefaultCredential_forProtectionSpace
  , setCredential_forProtectionSpace_task
  , removeCredential_forProtectionSpace_options_task
  , getDefaultCredentialForProtectionSpace_task_completionHandler
  , setDefaultCredential_forProtectionSpace_task
  , sharedCredentialStorage
  , allCredentials
  , credentialsForProtectionSpaceSelector
  , setCredential_forProtectionSpaceSelector
  , removeCredential_forProtectionSpaceSelector
  , removeCredential_forProtectionSpace_optionsSelector
  , defaultCredentialForProtectionSpaceSelector
  , setDefaultCredential_forProtectionSpaceSelector
  , setCredential_forProtectionSpace_taskSelector
  , removeCredential_forProtectionSpace_options_taskSelector
  , getDefaultCredentialForProtectionSpace_task_completionHandlerSelector
  , setDefaultCredential_forProtectionSpace_taskSelector
  , sharedCredentialStorageSelector
  , allCredentialsSelector


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

import ObjC.Foundation.Internal.Classes

-- | credentialsForProtectionSpace:
--
-- Get a dictionary mapping usernames to credentials for the specified protection space.
--
-- @space@ — An NSURLProtectionSpace indicating the protection space for which to get credentials
--
-- Returns: A dictionary where the keys are usernames and the values are the corresponding NSURLCredentials.
--
-- ObjC selector: @- credentialsForProtectionSpace:@
credentialsForProtectionSpace :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLProtectionSpace space) => nsurlCredentialStorage -> space -> IO (Id NSDictionary)
credentialsForProtectionSpace nsurlCredentialStorage  space =
withObjCPtr space $ \raw_space ->
    sendMsg nsurlCredentialStorage (mkSelector "credentialsForProtectionSpace:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ())] >>= retainedObject . castPtr

-- | setCredential:forProtectionSpace:
--
-- Add a new credential to the set for the specified protection space or replace an existing one.
--
-- @credential@ — The credential to set.
--
-- @space@ — The protection space for which to add it.
--
-- Multiple credentials may be set for a given protection space, but each must have    a distinct user. If a credential with the same user is already set for the protection space,    the new one will replace it.
--
-- ObjC selector: @- setCredential:forProtectionSpace:@
setCredential_forProtectionSpace :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace space) => nsurlCredentialStorage -> credential -> space -> IO ()
setCredential_forProtectionSpace nsurlCredentialStorage  credential space =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr space $ \raw_space ->
      sendMsg nsurlCredentialStorage (mkSelector "setCredential:forProtectionSpace:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_space :: Ptr ())]

-- | removeCredential:forProtectionSpace:
--
-- Remove the credential from the set for the specified protection space.
--
-- @credential@ — The credential to remove.
--
-- @space@ — The protection space for which a credential should be removed
--
-- The credential is removed from both persistent and temporary storage. A credential that    has a persistence policy of NSURLCredentialPersistenceSynchronizable will fail.      See removeCredential:forProtectionSpace:options.
--
-- ObjC selector: @- removeCredential:forProtectionSpace:@
removeCredential_forProtectionSpace :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace space) => nsurlCredentialStorage -> credential -> space -> IO ()
removeCredential_forProtectionSpace nsurlCredentialStorage  credential space =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr space $ \raw_space ->
      sendMsg nsurlCredentialStorage (mkSelector "removeCredential:forProtectionSpace:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_space :: Ptr ())]

-- | removeCredential:forProtectionSpace:options
--
-- Remove the credential from the set for the specified protection space based on options.
--
-- @credential@ — The credential to remove.
--
-- @space@ — The protection space for which a credential should be removed
--
-- @options@ — A dictionary containing options to consider when removing the credential.  This should be used when trying to delete a credential that has the NSURLCredentialPersistenceSynchronizable policy. Please note that when NSURLCredential objects that have a NSURLCredentialPersistenceSynchronizable policy are removed, the credential will be removed on all devices that contain this credential.
--
-- The credential is removed from both persistent and temporary storage.
--
-- ObjC selector: @- removeCredential:forProtectionSpace:options:@
removeCredential_forProtectionSpace_options :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace space, IsNSDictionary options) => nsurlCredentialStorage -> credential -> space -> options -> IO ()
removeCredential_forProtectionSpace_options nsurlCredentialStorage  credential space options =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr space $ \raw_space ->
    withObjCPtr options $ \raw_options ->
        sendMsg nsurlCredentialStorage (mkSelector "removeCredential:forProtectionSpace:options:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_space :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | defaultCredentialForProtectionSpace:
--
-- Get the default credential for the specified protection space.
--
-- @space@ — The protection space for which to get the default credential.
--
-- ObjC selector: @- defaultCredentialForProtectionSpace:@
defaultCredentialForProtectionSpace :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLProtectionSpace space) => nsurlCredentialStorage -> space -> IO (Id NSURLCredential)
defaultCredentialForProtectionSpace nsurlCredentialStorage  space =
withObjCPtr space $ \raw_space ->
    sendMsg nsurlCredentialStorage (mkSelector "defaultCredentialForProtectionSpace:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ())] >>= retainedObject . castPtr

-- | setDefaultCredential:forProtectionSpace:
--
-- Set the default credential for the specified protection space.
--
-- @credential@ — The credential to set as default.
--
-- @space@ — The protection space for which the credential should be set as default.
--
-- If the credential is not yet in the set for the protection space, it will be added to it.
--
-- ObjC selector: @- setDefaultCredential:forProtectionSpace:@
setDefaultCredential_forProtectionSpace :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace space) => nsurlCredentialStorage -> credential -> space -> IO ()
setDefaultCredential_forProtectionSpace nsurlCredentialStorage  credential space =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr space $ \raw_space ->
      sendMsg nsurlCredentialStorage (mkSelector "setDefaultCredential:forProtectionSpace:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_space :: Ptr ())]

-- | @- setCredential:forProtectionSpace:task:@
setCredential_forProtectionSpace_task :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace protectionSpace, IsNSURLSessionTask task) => nsurlCredentialStorage -> credential -> protectionSpace -> task -> IO ()
setCredential_forProtectionSpace_task nsurlCredentialStorage  credential protectionSpace task =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr protectionSpace $ \raw_protectionSpace ->
    withObjCPtr task $ \raw_task ->
        sendMsg nsurlCredentialStorage (mkSelector "setCredential:forProtectionSpace:task:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_protectionSpace :: Ptr ()), argPtr (castPtr raw_task :: Ptr ())]

-- | @- removeCredential:forProtectionSpace:options:task:@
removeCredential_forProtectionSpace_options_task :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace protectionSpace, IsNSDictionary options, IsNSURLSessionTask task) => nsurlCredentialStorage -> credential -> protectionSpace -> options -> task -> IO ()
removeCredential_forProtectionSpace_options_task nsurlCredentialStorage  credential protectionSpace options task =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr protectionSpace $ \raw_protectionSpace ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr task $ \raw_task ->
          sendMsg nsurlCredentialStorage (mkSelector "removeCredential:forProtectionSpace:options:task:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_protectionSpace :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_task :: Ptr ())]

-- | @- getDefaultCredentialForProtectionSpace:task:completionHandler:@
getDefaultCredentialForProtectionSpace_task_completionHandler :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLProtectionSpace space, IsNSURLSessionTask task) => nsurlCredentialStorage -> space -> task -> Ptr () -> IO ()
getDefaultCredentialForProtectionSpace_task_completionHandler nsurlCredentialStorage  space task completionHandler =
withObjCPtr space $ \raw_space ->
  withObjCPtr task $ \raw_task ->
      sendMsg nsurlCredentialStorage (mkSelector "getDefaultCredentialForProtectionSpace:task:completionHandler:") retVoid [argPtr (castPtr raw_space :: Ptr ()), argPtr (castPtr raw_task :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setDefaultCredential:forProtectionSpace:task:@
setDefaultCredential_forProtectionSpace_task :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace protectionSpace, IsNSURLSessionTask task) => nsurlCredentialStorage -> credential -> protectionSpace -> task -> IO ()
setDefaultCredential_forProtectionSpace_task nsurlCredentialStorage  credential protectionSpace task =
withObjCPtr credential $ \raw_credential ->
  withObjCPtr protectionSpace $ \raw_protectionSpace ->
    withObjCPtr task $ \raw_task ->
        sendMsg nsurlCredentialStorage (mkSelector "setDefaultCredential:forProtectionSpace:task:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_protectionSpace :: Ptr ()), argPtr (castPtr raw_task :: Ptr ())]

-- | sharedCredentialStorage
--
-- Get the shared singleton authentication storage
--
-- Returns: the shared authentication storage
--
-- ObjC selector: @+ sharedCredentialStorage@
sharedCredentialStorage :: IO (Id NSURLCredentialStorage)
sharedCredentialStorage  =
  do
    cls' <- getRequiredClass "NSURLCredentialStorage"
    sendClassMsg cls' (mkSelector "sharedCredentialStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get a dictionary mapping NSURLProtectionSpaces to dictionaries which map usernames to NSURLCredentials
--
-- Returns: an NSDictionary where the keys are NSURLProtectionSpaces    and the values are dictionaries, in which the keys are usernames    and the values are NSURLCredentials
--
-- ObjC selector: @- allCredentials@
allCredentials :: IsNSURLCredentialStorage nsurlCredentialStorage => nsurlCredentialStorage -> IO (Id NSDictionary)
allCredentials nsurlCredentialStorage  =
  sendMsg nsurlCredentialStorage (mkSelector "allCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @credentialsForProtectionSpace:@
credentialsForProtectionSpaceSelector :: Selector
credentialsForProtectionSpaceSelector = mkSelector "credentialsForProtectionSpace:"

-- | @Selector@ for @setCredential:forProtectionSpace:@
setCredential_forProtectionSpaceSelector :: Selector
setCredential_forProtectionSpaceSelector = mkSelector "setCredential:forProtectionSpace:"

-- | @Selector@ for @removeCredential:forProtectionSpace:@
removeCredential_forProtectionSpaceSelector :: Selector
removeCredential_forProtectionSpaceSelector = mkSelector "removeCredential:forProtectionSpace:"

-- | @Selector@ for @removeCredential:forProtectionSpace:options:@
removeCredential_forProtectionSpace_optionsSelector :: Selector
removeCredential_forProtectionSpace_optionsSelector = mkSelector "removeCredential:forProtectionSpace:options:"

-- | @Selector@ for @defaultCredentialForProtectionSpace:@
defaultCredentialForProtectionSpaceSelector :: Selector
defaultCredentialForProtectionSpaceSelector = mkSelector "defaultCredentialForProtectionSpace:"

-- | @Selector@ for @setDefaultCredential:forProtectionSpace:@
setDefaultCredential_forProtectionSpaceSelector :: Selector
setDefaultCredential_forProtectionSpaceSelector = mkSelector "setDefaultCredential:forProtectionSpace:"

-- | @Selector@ for @setCredential:forProtectionSpace:task:@
setCredential_forProtectionSpace_taskSelector :: Selector
setCredential_forProtectionSpace_taskSelector = mkSelector "setCredential:forProtectionSpace:task:"

-- | @Selector@ for @removeCredential:forProtectionSpace:options:task:@
removeCredential_forProtectionSpace_options_taskSelector :: Selector
removeCredential_forProtectionSpace_options_taskSelector = mkSelector "removeCredential:forProtectionSpace:options:task:"

-- | @Selector@ for @getDefaultCredentialForProtectionSpace:task:completionHandler:@
getDefaultCredentialForProtectionSpace_task_completionHandlerSelector :: Selector
getDefaultCredentialForProtectionSpace_task_completionHandlerSelector = mkSelector "getDefaultCredentialForProtectionSpace:task:completionHandler:"

-- | @Selector@ for @setDefaultCredential:forProtectionSpace:task:@
setDefaultCredential_forProtectionSpace_taskSelector :: Selector
setDefaultCredential_forProtectionSpace_taskSelector = mkSelector "setDefaultCredential:forProtectionSpace:task:"

-- | @Selector@ for @sharedCredentialStorage@
sharedCredentialStorageSelector :: Selector
sharedCredentialStorageSelector = mkSelector "sharedCredentialStorage"

-- | @Selector@ for @allCredentials@
allCredentialsSelector :: Selector
allCredentialsSelector = mkSelector "allCredentials"

