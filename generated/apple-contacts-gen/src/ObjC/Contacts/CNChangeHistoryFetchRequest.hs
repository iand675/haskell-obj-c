{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifies the criteria to fetch change history.
--
-- Changes to contacts are always returned.              All changes are coalesced to remove redundant adds, updates and deletes.              This request is used with [CNContactStore enumeratorForChangeHistoryFetchRequest:error:].
--
-- Generated bindings for @CNChangeHistoryFetchRequest@.
module ObjC.Contacts.CNChangeHistoryFetchRequest
  ( CNChangeHistoryFetchRequest
  , IsCNChangeHistoryFetchRequest(..)
  , startingToken
  , setStartingToken
  , additionalContactKeyDescriptors
  , setAdditionalContactKeyDescriptors
  , shouldUnifyResults
  , setShouldUnifyResults
  , mutableObjects
  , setMutableObjects
  , includeGroupChanges
  , setIncludeGroupChanges
  , excludedTransactionAuthors
  , setExcludedTransactionAuthors
  , startingTokenSelector
  , setStartingTokenSelector
  , additionalContactKeyDescriptorsSelector
  , setAdditionalContactKeyDescriptorsSelector
  , shouldUnifyResultsSelector
  , setShouldUnifyResultsSelector
  , mutableObjectsSelector
  , setMutableObjectsSelector
  , includeGroupChangesSelector
  , setIncludeGroupChangesSelector
  , excludedTransactionAuthorsSelector
  , setExcludedTransactionAuthorsSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Request changes made after a certain point.
--
-- If non-nil, only changes made after this point in history will be returned.
--
-- If nil, a @CNChangeHistoryDropEverythingEvent@ will be returned, followed by an add event              for every contact and group currently in the contacts database.
--
-- ObjC selector: @- startingToken@
startingToken :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO (Id NSData)
startingToken cnChangeHistoryFetchRequest  =
    sendMsg cnChangeHistoryFetchRequest (mkSelector "startingToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Request changes made after a certain point.
--
-- If non-nil, only changes made after this point in history will be returned.
--
-- If nil, a @CNChangeHistoryDropEverythingEvent@ will be returned, followed by an add event              for every contact and group currently in the contacts database.
--
-- ObjC selector: @- setStartingToken:@
setStartingToken :: (IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest, IsNSData value) => cnChangeHistoryFetchRequest -> value -> IO ()
setStartingToken cnChangeHistoryFetchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cnChangeHistoryFetchRequest (mkSelector "setStartingToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Additional keys to include in the fetched contacts.
--
-- By default, only @CNContactIdentifierKey@ will be fetched. If you              would like to include additional key descriptors to process the contacts,              include the key descriptors you need.
--
-- @CNContactIdentifierKey@ will always be fetched, whether you              request it or not.
--
-- ObjC selector: @- additionalContactKeyDescriptors@
additionalContactKeyDescriptors :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO (Id NSArray)
additionalContactKeyDescriptors cnChangeHistoryFetchRequest  =
    sendMsg cnChangeHistoryFetchRequest (mkSelector "additionalContactKeyDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Additional keys to include in the fetched contacts.
--
-- By default, only @CNContactIdentifierKey@ will be fetched. If you              would like to include additional key descriptors to process the contacts,              include the key descriptors you need.
--
-- @CNContactIdentifierKey@ will always be fetched, whether you              request it or not.
--
-- ObjC selector: @- setAdditionalContactKeyDescriptors:@
setAdditionalContactKeyDescriptors :: (IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest, IsNSArray value) => cnChangeHistoryFetchRequest -> value -> IO ()
setAdditionalContactKeyDescriptors cnChangeHistoryFetchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cnChangeHistoryFetchRequest (mkSelector "setAdditionalContactKeyDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Returns contact changes as unified contacts.
--
-- If @YES,@ returns unified contact history. Otherwise returns individual contact history. Default is @YES.@
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts.              If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- shouldUnifyResults@
shouldUnifyResults :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO Bool
shouldUnifyResults cnChangeHistoryFetchRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnChangeHistoryFetchRequest (mkSelector "shouldUnifyResults") retCULong []

-- | Returns contact changes as unified contacts.
--
-- If @YES,@ returns unified contact history. Otherwise returns individual contact history. Default is @YES.@
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts.              If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- setShouldUnifyResults:@
setShouldUnifyResults :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> Bool -> IO ()
setShouldUnifyResults cnChangeHistoryFetchRequest  value =
    sendMsg cnChangeHistoryFetchRequest (mkSelector "setShouldUnifyResults:") retVoid [argCULong (if value then 1 else 0)]

-- | To return mutable contacts and groups.
--
-- If @YES@ returns mutable contacts and groups. Default is @NO.@
--
-- ObjC selector: @- mutableObjects@
mutableObjects :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO Bool
mutableObjects cnChangeHistoryFetchRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnChangeHistoryFetchRequest (mkSelector "mutableObjects") retCULong []

-- | To return mutable contacts and groups.
--
-- If @YES@ returns mutable contacts and groups. Default is @NO.@
--
-- ObjC selector: @- setMutableObjects:@
setMutableObjects :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> Bool -> IO ()
setMutableObjects cnChangeHistoryFetchRequest  value =
    sendMsg cnChangeHistoryFetchRequest (mkSelector "setMutableObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | Set to @YES@ to also fetch group changes. Default is @NO.@
--
-- ObjC selector: @- includeGroupChanges@
includeGroupChanges :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO Bool
includeGroupChanges cnChangeHistoryFetchRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnChangeHistoryFetchRequest (mkSelector "includeGroupChanges") retCULong []

-- | Set to @YES@ to also fetch group changes. Default is @NO.@
--
-- ObjC selector: @- setIncludeGroupChanges:@
setIncludeGroupChanges :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> Bool -> IO ()
setIncludeGroupChanges cnChangeHistoryFetchRequest  value =
    sendMsg cnChangeHistoryFetchRequest (mkSelector "setIncludeGroupChanges:") retVoid [argCULong (if value then 1 else 0)]

-- | Exclude changes made by certain authors.
--
-- If set, transactions made by the specified authors will be excluded              from the results. Use this, in conjunction with @CNSaveRequest.transactionAuthor,@              to suppress processing of changes you already know about.
--
-- ObjC selector: @- excludedTransactionAuthors@
excludedTransactionAuthors :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO (Id NSArray)
excludedTransactionAuthors cnChangeHistoryFetchRequest  =
    sendMsg cnChangeHistoryFetchRequest (mkSelector "excludedTransactionAuthors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Exclude changes made by certain authors.
--
-- If set, transactions made by the specified authors will be excluded              from the results. Use this, in conjunction with @CNSaveRequest.transactionAuthor,@              to suppress processing of changes you already know about.
--
-- ObjC selector: @- setExcludedTransactionAuthors:@
setExcludedTransactionAuthors :: (IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest, IsNSArray value) => cnChangeHistoryFetchRequest -> value -> IO ()
setExcludedTransactionAuthors cnChangeHistoryFetchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cnChangeHistoryFetchRequest (mkSelector "setExcludedTransactionAuthors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startingToken@
startingTokenSelector :: Selector
startingTokenSelector = mkSelector "startingToken"

-- | @Selector@ for @setStartingToken:@
setStartingTokenSelector :: Selector
setStartingTokenSelector = mkSelector "setStartingToken:"

-- | @Selector@ for @additionalContactKeyDescriptors@
additionalContactKeyDescriptorsSelector :: Selector
additionalContactKeyDescriptorsSelector = mkSelector "additionalContactKeyDescriptors"

-- | @Selector@ for @setAdditionalContactKeyDescriptors:@
setAdditionalContactKeyDescriptorsSelector :: Selector
setAdditionalContactKeyDescriptorsSelector = mkSelector "setAdditionalContactKeyDescriptors:"

-- | @Selector@ for @shouldUnifyResults@
shouldUnifyResultsSelector :: Selector
shouldUnifyResultsSelector = mkSelector "shouldUnifyResults"

-- | @Selector@ for @setShouldUnifyResults:@
setShouldUnifyResultsSelector :: Selector
setShouldUnifyResultsSelector = mkSelector "setShouldUnifyResults:"

-- | @Selector@ for @mutableObjects@
mutableObjectsSelector :: Selector
mutableObjectsSelector = mkSelector "mutableObjects"

-- | @Selector@ for @setMutableObjects:@
setMutableObjectsSelector :: Selector
setMutableObjectsSelector = mkSelector "setMutableObjects:"

-- | @Selector@ for @includeGroupChanges@
includeGroupChangesSelector :: Selector
includeGroupChangesSelector = mkSelector "includeGroupChanges"

-- | @Selector@ for @setIncludeGroupChanges:@
setIncludeGroupChangesSelector :: Selector
setIncludeGroupChangesSelector = mkSelector "setIncludeGroupChanges:"

-- | @Selector@ for @excludedTransactionAuthors@
excludedTransactionAuthorsSelector :: Selector
excludedTransactionAuthorsSelector = mkSelector "excludedTransactionAuthors"

-- | @Selector@ for @setExcludedTransactionAuthors:@
setExcludedTransactionAuthorsSelector :: Selector
setExcludedTransactionAuthorsSelector = mkSelector "setExcludedTransactionAuthors:"

