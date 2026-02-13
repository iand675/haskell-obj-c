{-# LANGUAGE DataKinds #-}
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
  , allCredentialsSelector
  , credentialsForProtectionSpaceSelector
  , defaultCredentialForProtectionSpaceSelector
  , getDefaultCredentialForProtectionSpace_task_completionHandlerSelector
  , removeCredential_forProtectionSpaceSelector
  , removeCredential_forProtectionSpace_optionsSelector
  , removeCredential_forProtectionSpace_options_taskSelector
  , setCredential_forProtectionSpaceSelector
  , setCredential_forProtectionSpace_taskSelector
  , setDefaultCredential_forProtectionSpaceSelector
  , setDefaultCredential_forProtectionSpace_taskSelector
  , sharedCredentialStorageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
credentialsForProtectionSpace nsurlCredentialStorage space =
  sendMessage nsurlCredentialStorage credentialsForProtectionSpaceSelector (toNSURLProtectionSpace space)

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
setCredential_forProtectionSpace nsurlCredentialStorage credential space =
  sendMessage nsurlCredentialStorage setCredential_forProtectionSpaceSelector (toNSURLCredential credential) (toNSURLProtectionSpace space)

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
removeCredential_forProtectionSpace nsurlCredentialStorage credential space =
  sendMessage nsurlCredentialStorage removeCredential_forProtectionSpaceSelector (toNSURLCredential credential) (toNSURLProtectionSpace space)

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
removeCredential_forProtectionSpace_options nsurlCredentialStorage credential space options =
  sendMessage nsurlCredentialStorage removeCredential_forProtectionSpace_optionsSelector (toNSURLCredential credential) (toNSURLProtectionSpace space) (toNSDictionary options)

-- | defaultCredentialForProtectionSpace:
--
-- Get the default credential for the specified protection space.
--
-- @space@ — The protection space for which to get the default credential.
--
-- ObjC selector: @- defaultCredentialForProtectionSpace:@
defaultCredentialForProtectionSpace :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLProtectionSpace space) => nsurlCredentialStorage -> space -> IO (Id NSURLCredential)
defaultCredentialForProtectionSpace nsurlCredentialStorage space =
  sendMessage nsurlCredentialStorage defaultCredentialForProtectionSpaceSelector (toNSURLProtectionSpace space)

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
setDefaultCredential_forProtectionSpace nsurlCredentialStorage credential space =
  sendMessage nsurlCredentialStorage setDefaultCredential_forProtectionSpaceSelector (toNSURLCredential credential) (toNSURLProtectionSpace space)

-- | @- setCredential:forProtectionSpace:task:@
setCredential_forProtectionSpace_task :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace protectionSpace, IsNSURLSessionTask task) => nsurlCredentialStorage -> credential -> protectionSpace -> task -> IO ()
setCredential_forProtectionSpace_task nsurlCredentialStorage credential protectionSpace task =
  sendMessage nsurlCredentialStorage setCredential_forProtectionSpace_taskSelector (toNSURLCredential credential) (toNSURLProtectionSpace protectionSpace) (toNSURLSessionTask task)

-- | @- removeCredential:forProtectionSpace:options:task:@
removeCredential_forProtectionSpace_options_task :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace protectionSpace, IsNSDictionary options, IsNSURLSessionTask task) => nsurlCredentialStorage -> credential -> protectionSpace -> options -> task -> IO ()
removeCredential_forProtectionSpace_options_task nsurlCredentialStorage credential protectionSpace options task =
  sendMessage nsurlCredentialStorage removeCredential_forProtectionSpace_options_taskSelector (toNSURLCredential credential) (toNSURLProtectionSpace protectionSpace) (toNSDictionary options) (toNSURLSessionTask task)

-- | @- getDefaultCredentialForProtectionSpace:task:completionHandler:@
getDefaultCredentialForProtectionSpace_task_completionHandler :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLProtectionSpace space, IsNSURLSessionTask task) => nsurlCredentialStorage -> space -> task -> Ptr () -> IO ()
getDefaultCredentialForProtectionSpace_task_completionHandler nsurlCredentialStorage space task completionHandler =
  sendMessage nsurlCredentialStorage getDefaultCredentialForProtectionSpace_task_completionHandlerSelector (toNSURLProtectionSpace space) (toNSURLSessionTask task) completionHandler

-- | @- setDefaultCredential:forProtectionSpace:task:@
setDefaultCredential_forProtectionSpace_task :: (IsNSURLCredentialStorage nsurlCredentialStorage, IsNSURLCredential credential, IsNSURLProtectionSpace protectionSpace, IsNSURLSessionTask task) => nsurlCredentialStorage -> credential -> protectionSpace -> task -> IO ()
setDefaultCredential_forProtectionSpace_task nsurlCredentialStorage credential protectionSpace task =
  sendMessage nsurlCredentialStorage setDefaultCredential_forProtectionSpace_taskSelector (toNSURLCredential credential) (toNSURLProtectionSpace protectionSpace) (toNSURLSessionTask task)

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
    sendClassMessage cls' sharedCredentialStorageSelector

-- | Get a dictionary mapping NSURLProtectionSpaces to dictionaries which map usernames to NSURLCredentials
--
-- Returns: an NSDictionary where the keys are NSURLProtectionSpaces    and the values are dictionaries, in which the keys are usernames    and the values are NSURLCredentials
--
-- ObjC selector: @- allCredentials@
allCredentials :: IsNSURLCredentialStorage nsurlCredentialStorage => nsurlCredentialStorage -> IO (Id NSDictionary)
allCredentials nsurlCredentialStorage =
  sendMessage nsurlCredentialStorage allCredentialsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @credentialsForProtectionSpace:@
credentialsForProtectionSpaceSelector :: Selector '[Id NSURLProtectionSpace] (Id NSDictionary)
credentialsForProtectionSpaceSelector = mkSelector "credentialsForProtectionSpace:"

-- | @Selector@ for @setCredential:forProtectionSpace:@
setCredential_forProtectionSpaceSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace] ()
setCredential_forProtectionSpaceSelector = mkSelector "setCredential:forProtectionSpace:"

-- | @Selector@ for @removeCredential:forProtectionSpace:@
removeCredential_forProtectionSpaceSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace] ()
removeCredential_forProtectionSpaceSelector = mkSelector "removeCredential:forProtectionSpace:"

-- | @Selector@ for @removeCredential:forProtectionSpace:options:@
removeCredential_forProtectionSpace_optionsSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace, Id NSDictionary] ()
removeCredential_forProtectionSpace_optionsSelector = mkSelector "removeCredential:forProtectionSpace:options:"

-- | @Selector@ for @defaultCredentialForProtectionSpace:@
defaultCredentialForProtectionSpaceSelector :: Selector '[Id NSURLProtectionSpace] (Id NSURLCredential)
defaultCredentialForProtectionSpaceSelector = mkSelector "defaultCredentialForProtectionSpace:"

-- | @Selector@ for @setDefaultCredential:forProtectionSpace:@
setDefaultCredential_forProtectionSpaceSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace] ()
setDefaultCredential_forProtectionSpaceSelector = mkSelector "setDefaultCredential:forProtectionSpace:"

-- | @Selector@ for @setCredential:forProtectionSpace:task:@
setCredential_forProtectionSpace_taskSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace, Id NSURLSessionTask] ()
setCredential_forProtectionSpace_taskSelector = mkSelector "setCredential:forProtectionSpace:task:"

-- | @Selector@ for @removeCredential:forProtectionSpace:options:task:@
removeCredential_forProtectionSpace_options_taskSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace, Id NSDictionary, Id NSURLSessionTask] ()
removeCredential_forProtectionSpace_options_taskSelector = mkSelector "removeCredential:forProtectionSpace:options:task:"

-- | @Selector@ for @getDefaultCredentialForProtectionSpace:task:completionHandler:@
getDefaultCredentialForProtectionSpace_task_completionHandlerSelector :: Selector '[Id NSURLProtectionSpace, Id NSURLSessionTask, Ptr ()] ()
getDefaultCredentialForProtectionSpace_task_completionHandlerSelector = mkSelector "getDefaultCredentialForProtectionSpace:task:completionHandler:"

-- | @Selector@ for @setDefaultCredential:forProtectionSpace:task:@
setDefaultCredential_forProtectionSpace_taskSelector :: Selector '[Id NSURLCredential, Id NSURLProtectionSpace, Id NSURLSessionTask] ()
setDefaultCredential_forProtectionSpace_taskSelector = mkSelector "setDefaultCredential:forProtectionSpace:task:"

-- | @Selector@ for @sharedCredentialStorage@
sharedCredentialStorageSelector :: Selector '[] (Id NSURLCredentialStorage)
sharedCredentialStorageSelector = mkSelector "sharedCredentialStorage"

-- | @Selector@ for @allCredentials@
allCredentialsSelector :: Selector '[] (Id NSDictionary)
allCredentialsSelector = mkSelector "allCredentials"

