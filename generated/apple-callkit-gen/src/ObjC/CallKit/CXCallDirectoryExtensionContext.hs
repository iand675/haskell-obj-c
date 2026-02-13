{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallDirectoryExtensionContext@.
module ObjC.CallKit.CXCallDirectoryExtensionContext
  ( CXCallDirectoryExtensionContext
  , IsCXCallDirectoryExtensionContext(..)
  , addBlockingEntryWithNextSequentialPhoneNumber
  , removeBlockingEntryWithPhoneNumber
  , removeAllBlockingEntries
  , addIdentificationEntryWithNextSequentialPhoneNumber_label
  , removeIdentificationEntryWithPhoneNumber
  , removeAllIdentificationEntries
  , completeRequestWithCompletionHandler
  , completeRequestReturningItems_completionHandler
  , delegate
  , setDelegate
  , incremental
  , addBlockingEntryWithNextSequentialPhoneNumberSelector
  , addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector
  , completeRequestReturningItems_completionHandlerSelector
  , completeRequestWithCompletionHandlerSelector
  , delegateSelector
  , incrementalSelector
  , removeAllBlockingEntriesSelector
  , removeAllIdentificationEntriesSelector
  , removeBlockingEntryWithPhoneNumberSelector
  , removeIdentificationEntryWithPhoneNumberSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addBlockingEntryWithNextSequentialPhoneNumber:@
addBlockingEntryWithNextSequentialPhoneNumber :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> CLong -> IO ()
addBlockingEntryWithNextSequentialPhoneNumber cxCallDirectoryExtensionContext phoneNumber =
  sendMessage cxCallDirectoryExtensionContext addBlockingEntryWithNextSequentialPhoneNumberSelector phoneNumber

-- | Remove blocking entry with the specified phone number.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove a previously-added blocking entry.
--
-- @phoneNumber@ — The blocking entry phone number to remove.
--
-- ObjC selector: @- removeBlockingEntryWithPhoneNumber:@
removeBlockingEntryWithPhoneNumber :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> CLong -> IO ()
removeBlockingEntryWithPhoneNumber cxCallDirectoryExtensionContext phoneNumber =
  sendMessage cxCallDirectoryExtensionContext removeBlockingEntryWithPhoneNumberSelector phoneNumber

-- | Remove all currently-stored blocking entries.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove all previously-added blocking entries.
--
-- ObjC selector: @- removeAllBlockingEntries@
removeAllBlockingEntries :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO ()
removeAllBlockingEntries cxCallDirectoryExtensionContext =
  sendMessage cxCallDirectoryExtensionContext removeAllBlockingEntriesSelector

-- | @- addIdentificationEntryWithNextSequentialPhoneNumber:label:@
addIdentificationEntryWithNextSequentialPhoneNumber_label :: (IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext, IsNSString label) => cxCallDirectoryExtensionContext -> CLong -> label -> IO ()
addIdentificationEntryWithNextSequentialPhoneNumber_label cxCallDirectoryExtensionContext phoneNumber label =
  sendMessage cxCallDirectoryExtensionContext addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector phoneNumber (toNSString label)

-- | Remove identification entry with the specified phone number.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove a previously-added identification entry. Removes all identification entries with the specified phone number, even if multiple identification entries with different labels are present for a single phone number.
--
-- @phoneNumber@ — The identification entry phone number to remove.
--
-- ObjC selector: @- removeIdentificationEntryWithPhoneNumber:@
removeIdentificationEntryWithPhoneNumber :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> CLong -> IO ()
removeIdentificationEntryWithPhoneNumber cxCallDirectoryExtensionContext phoneNumber =
  sendMessage cxCallDirectoryExtensionContext removeIdentificationEntryWithPhoneNumberSelector phoneNumber

-- | Remove all currently-stored identification entries.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove all previously-added identification entries.
--
-- ObjC selector: @- removeAllIdentificationEntries@
removeAllIdentificationEntries :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO ()
removeAllIdentificationEntries cxCallDirectoryExtensionContext =
  sendMessage cxCallDirectoryExtensionContext removeAllIdentificationEntriesSelector

-- | @- completeRequestWithCompletionHandler:@
completeRequestWithCompletionHandler :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> Ptr () -> IO ()
completeRequestWithCompletionHandler cxCallDirectoryExtensionContext completion =
  sendMessage cxCallDirectoryExtensionContext completeRequestWithCompletionHandlerSelector completion

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext, IsNSArray items) => cxCallDirectoryExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler cxCallDirectoryExtensionContext items completionHandler =
  sendMessage cxCallDirectoryExtensionContext completeRequestReturningItems_completionHandlerSelector (toNSArray items) completionHandler

-- | @- delegate@
delegate :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO RawId
delegate cxCallDirectoryExtensionContext =
  sendMessage cxCallDirectoryExtensionContext delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> RawId -> IO ()
setDelegate cxCallDirectoryExtensionContext value =
  sendMessage cxCallDirectoryExtensionContext setDelegateSelector value

-- | Whether the request should provide incremental data.
--
-- If this is called at the beginning of the request (before any entries have been added or removed) and the result is YES, then the request must only provide an "incremental" set of entries, i.e. only add or remove entries relative to the last time data was loaded for the extension. Otherwise, if this method is not called OR is called and returns NO, then the request must provide a "complete" set of entries, adding the full list of entries from scratch (and removing none), regardless of whether data has ever been successfully loaded in the past.
--
-- ObjC selector: @- incremental@
incremental :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO Bool
incremental cxCallDirectoryExtensionContext =
  sendMessage cxCallDirectoryExtensionContext incrementalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addBlockingEntryWithNextSequentialPhoneNumber:@
addBlockingEntryWithNextSequentialPhoneNumberSelector :: Selector '[CLong] ()
addBlockingEntryWithNextSequentialPhoneNumberSelector = mkSelector "addBlockingEntryWithNextSequentialPhoneNumber:"

-- | @Selector@ for @removeBlockingEntryWithPhoneNumber:@
removeBlockingEntryWithPhoneNumberSelector :: Selector '[CLong] ()
removeBlockingEntryWithPhoneNumberSelector = mkSelector "removeBlockingEntryWithPhoneNumber:"

-- | @Selector@ for @removeAllBlockingEntries@
removeAllBlockingEntriesSelector :: Selector '[] ()
removeAllBlockingEntriesSelector = mkSelector "removeAllBlockingEntries"

-- | @Selector@ for @addIdentificationEntryWithNextSequentialPhoneNumber:label:@
addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector :: Selector '[CLong, Id NSString] ()
addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector = mkSelector "addIdentificationEntryWithNextSequentialPhoneNumber:label:"

-- | @Selector@ for @removeIdentificationEntryWithPhoneNumber:@
removeIdentificationEntryWithPhoneNumberSelector :: Selector '[CLong] ()
removeIdentificationEntryWithPhoneNumberSelector = mkSelector "removeIdentificationEntryWithPhoneNumber:"

-- | @Selector@ for @removeAllIdentificationEntries@
removeAllIdentificationEntriesSelector :: Selector '[] ()
removeAllIdentificationEntriesSelector = mkSelector "removeAllIdentificationEntries"

-- | @Selector@ for @completeRequestWithCompletionHandler:@
completeRequestWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
completeRequestWithCompletionHandlerSelector = mkSelector "completeRequestWithCompletionHandler:"

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @incremental@
incrementalSelector :: Selector '[] Bool
incrementalSelector = mkSelector "incremental"

