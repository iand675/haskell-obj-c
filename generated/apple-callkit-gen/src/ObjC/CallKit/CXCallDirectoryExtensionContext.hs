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
  , removeBlockingEntryWithPhoneNumberSelector
  , removeAllBlockingEntriesSelector
  , addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector
  , removeIdentificationEntryWithPhoneNumberSelector
  , removeAllIdentificationEntriesSelector
  , completeRequestWithCompletionHandlerSelector
  , completeRequestReturningItems_completionHandlerSelector
  , delegateSelector
  , setDelegateSelector
  , incrementalSelector


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

-- | @- addBlockingEntryWithNextSequentialPhoneNumber:@
addBlockingEntryWithNextSequentialPhoneNumber :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> CLong -> IO ()
addBlockingEntryWithNextSequentialPhoneNumber cxCallDirectoryExtensionContext  phoneNumber =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "addBlockingEntryWithNextSequentialPhoneNumber:") retVoid [argCLong phoneNumber]

-- | Remove blocking entry with the specified phone number.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove a previously-added blocking entry.
--
-- @phoneNumber@ — The blocking entry phone number to remove.
--
-- ObjC selector: @- removeBlockingEntryWithPhoneNumber:@
removeBlockingEntryWithPhoneNumber :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> CLong -> IO ()
removeBlockingEntryWithPhoneNumber cxCallDirectoryExtensionContext  phoneNumber =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "removeBlockingEntryWithPhoneNumber:") retVoid [argCLong phoneNumber]

-- | Remove all currently-stored blocking entries.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove all previously-added blocking entries.
--
-- ObjC selector: @- removeAllBlockingEntries@
removeAllBlockingEntries :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO ()
removeAllBlockingEntries cxCallDirectoryExtensionContext  =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "removeAllBlockingEntries") retVoid []

-- | @- addIdentificationEntryWithNextSequentialPhoneNumber:label:@
addIdentificationEntryWithNextSequentialPhoneNumber_label :: (IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext, IsNSString label) => cxCallDirectoryExtensionContext -> CLong -> label -> IO ()
addIdentificationEntryWithNextSequentialPhoneNumber_label cxCallDirectoryExtensionContext  phoneNumber label =
  withObjCPtr label $ \raw_label ->
      sendMsg cxCallDirectoryExtensionContext (mkSelector "addIdentificationEntryWithNextSequentialPhoneNumber:label:") retVoid [argCLong phoneNumber, argPtr (castPtr raw_label :: Ptr ())]

-- | Remove identification entry with the specified phone number.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove a previously-added identification entry. Removes all identification entries with the specified phone number, even if multiple identification entries with different labels are present for a single phone number.
--
-- @phoneNumber@ — The identification entry phone number to remove.
--
-- ObjC selector: @- removeIdentificationEntryWithPhoneNumber:@
removeIdentificationEntryWithPhoneNumber :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> CLong -> IO ()
removeIdentificationEntryWithPhoneNumber cxCallDirectoryExtensionContext  phoneNumber =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "removeIdentificationEntryWithPhoneNumber:") retVoid [argCLong phoneNumber]

-- | Remove all currently-stored identification entries.
--
-- May only be used when @-isIncremental@ returns YES, indicating that the request should provide incremental entries and thus may use this API to remove all previously-added identification entries.
--
-- ObjC selector: @- removeAllIdentificationEntries@
removeAllIdentificationEntries :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO ()
removeAllIdentificationEntries cxCallDirectoryExtensionContext  =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "removeAllIdentificationEntries") retVoid []

-- | @- completeRequestWithCompletionHandler:@
completeRequestWithCompletionHandler :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> Ptr () -> IO ()
completeRequestWithCompletionHandler cxCallDirectoryExtensionContext  completion =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "completeRequestWithCompletionHandler:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext, IsNSArray items) => cxCallDirectoryExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler cxCallDirectoryExtensionContext  items completionHandler =
  withObjCPtr items $ \raw_items ->
      sendMsg cxCallDirectoryExtensionContext (mkSelector "completeRequestReturningItems:completionHandler:") retVoid [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- delegate@
delegate :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO RawId
delegate cxCallDirectoryExtensionContext  =
    fmap (RawId . castPtr) $ sendMsg cxCallDirectoryExtensionContext (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> RawId -> IO ()
setDelegate cxCallDirectoryExtensionContext  value =
    sendMsg cxCallDirectoryExtensionContext (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Whether the request should provide incremental data.
--
-- If this is called at the beginning of the request (before any entries have been added or removed) and the result is YES, then the request must only provide an "incremental" set of entries, i.e. only add or remove entries relative to the last time data was loaded for the extension. Otherwise, if this method is not called OR is called and returns NO, then the request must provide a "complete" set of entries, adding the full list of entries from scratch (and removing none), regardless of whether data has ever been successfully loaded in the past.
--
-- ObjC selector: @- incremental@
incremental :: IsCXCallDirectoryExtensionContext cxCallDirectoryExtensionContext => cxCallDirectoryExtensionContext -> IO Bool
incremental cxCallDirectoryExtensionContext  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxCallDirectoryExtensionContext (mkSelector "incremental") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addBlockingEntryWithNextSequentialPhoneNumber:@
addBlockingEntryWithNextSequentialPhoneNumberSelector :: Selector
addBlockingEntryWithNextSequentialPhoneNumberSelector = mkSelector "addBlockingEntryWithNextSequentialPhoneNumber:"

-- | @Selector@ for @removeBlockingEntryWithPhoneNumber:@
removeBlockingEntryWithPhoneNumberSelector :: Selector
removeBlockingEntryWithPhoneNumberSelector = mkSelector "removeBlockingEntryWithPhoneNumber:"

-- | @Selector@ for @removeAllBlockingEntries@
removeAllBlockingEntriesSelector :: Selector
removeAllBlockingEntriesSelector = mkSelector "removeAllBlockingEntries"

-- | @Selector@ for @addIdentificationEntryWithNextSequentialPhoneNumber:label:@
addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector :: Selector
addIdentificationEntryWithNextSequentialPhoneNumber_labelSelector = mkSelector "addIdentificationEntryWithNextSequentialPhoneNumber:label:"

-- | @Selector@ for @removeIdentificationEntryWithPhoneNumber:@
removeIdentificationEntryWithPhoneNumberSelector :: Selector
removeIdentificationEntryWithPhoneNumberSelector = mkSelector "removeIdentificationEntryWithPhoneNumber:"

-- | @Selector@ for @removeAllIdentificationEntries@
removeAllIdentificationEntriesSelector :: Selector
removeAllIdentificationEntriesSelector = mkSelector "removeAllIdentificationEntries"

-- | @Selector@ for @completeRequestWithCompletionHandler:@
completeRequestWithCompletionHandlerSelector :: Selector
completeRequestWithCompletionHandlerSelector = mkSelector "completeRequestWithCompletionHandler:"

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @incremental@
incrementalSelector :: Selector
incrementalSelector = mkSelector "incremental"

