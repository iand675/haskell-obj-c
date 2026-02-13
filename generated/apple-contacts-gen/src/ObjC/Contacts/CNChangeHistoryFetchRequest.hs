{-# LANGUAGE DataKinds #-}
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
  , additionalContactKeyDescriptorsSelector
  , excludedTransactionAuthorsSelector
  , includeGroupChangesSelector
  , mutableObjectsSelector
  , setAdditionalContactKeyDescriptorsSelector
  , setExcludedTransactionAuthorsSelector
  , setIncludeGroupChangesSelector
  , setMutableObjectsSelector
  , setShouldUnifyResultsSelector
  , setStartingTokenSelector
  , shouldUnifyResultsSelector
  , startingTokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
startingToken cnChangeHistoryFetchRequest =
  sendMessage cnChangeHistoryFetchRequest startingTokenSelector

-- | Request changes made after a certain point.
--
-- If non-nil, only changes made after this point in history will be returned.
--
-- If nil, a @CNChangeHistoryDropEverythingEvent@ will be returned, followed by an add event              for every contact and group currently in the contacts database.
--
-- ObjC selector: @- setStartingToken:@
setStartingToken :: (IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest, IsNSData value) => cnChangeHistoryFetchRequest -> value -> IO ()
setStartingToken cnChangeHistoryFetchRequest value =
  sendMessage cnChangeHistoryFetchRequest setStartingTokenSelector (toNSData value)

-- | Additional keys to include in the fetched contacts.
--
-- By default, only @CNContactIdentifierKey@ will be fetched. If you              would like to include additional key descriptors to process the contacts,              include the key descriptors you need.
--
-- @CNContactIdentifierKey@ will always be fetched, whether you              request it or not.
--
-- ObjC selector: @- additionalContactKeyDescriptors@
additionalContactKeyDescriptors :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO (Id NSArray)
additionalContactKeyDescriptors cnChangeHistoryFetchRequest =
  sendMessage cnChangeHistoryFetchRequest additionalContactKeyDescriptorsSelector

-- | Additional keys to include in the fetched contacts.
--
-- By default, only @CNContactIdentifierKey@ will be fetched. If you              would like to include additional key descriptors to process the contacts,              include the key descriptors you need.
--
-- @CNContactIdentifierKey@ will always be fetched, whether you              request it or not.
--
-- ObjC selector: @- setAdditionalContactKeyDescriptors:@
setAdditionalContactKeyDescriptors :: (IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest, IsNSArray value) => cnChangeHistoryFetchRequest -> value -> IO ()
setAdditionalContactKeyDescriptors cnChangeHistoryFetchRequest value =
  sendMessage cnChangeHistoryFetchRequest setAdditionalContactKeyDescriptorsSelector (toNSArray value)

-- | Returns contact changes as unified contacts.
--
-- If @YES,@ returns unified contact history. Otherwise returns individual contact history. Default is @YES.@
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts.              If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- shouldUnifyResults@
shouldUnifyResults :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO Bool
shouldUnifyResults cnChangeHistoryFetchRequest =
  sendMessage cnChangeHistoryFetchRequest shouldUnifyResultsSelector

-- | Returns contact changes as unified contacts.
--
-- If @YES,@ returns unified contact history. Otherwise returns individual contact history. Default is @YES.@
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts.              If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- setShouldUnifyResults:@
setShouldUnifyResults :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> Bool -> IO ()
setShouldUnifyResults cnChangeHistoryFetchRequest value =
  sendMessage cnChangeHistoryFetchRequest setShouldUnifyResultsSelector value

-- | To return mutable contacts and groups.
--
-- If @YES@ returns mutable contacts and groups. Default is @NO.@
--
-- ObjC selector: @- mutableObjects@
mutableObjects :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO Bool
mutableObjects cnChangeHistoryFetchRequest =
  sendMessage cnChangeHistoryFetchRequest mutableObjectsSelector

-- | To return mutable contacts and groups.
--
-- If @YES@ returns mutable contacts and groups. Default is @NO.@
--
-- ObjC selector: @- setMutableObjects:@
setMutableObjects :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> Bool -> IO ()
setMutableObjects cnChangeHistoryFetchRequest value =
  sendMessage cnChangeHistoryFetchRequest setMutableObjectsSelector value

-- | Set to @YES@ to also fetch group changes. Default is @NO.@
--
-- ObjC selector: @- includeGroupChanges@
includeGroupChanges :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO Bool
includeGroupChanges cnChangeHistoryFetchRequest =
  sendMessage cnChangeHistoryFetchRequest includeGroupChangesSelector

-- | Set to @YES@ to also fetch group changes. Default is @NO.@
--
-- ObjC selector: @- setIncludeGroupChanges:@
setIncludeGroupChanges :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> Bool -> IO ()
setIncludeGroupChanges cnChangeHistoryFetchRequest value =
  sendMessage cnChangeHistoryFetchRequest setIncludeGroupChangesSelector value

-- | Exclude changes made by certain authors.
--
-- If set, transactions made by the specified authors will be excluded              from the results. Use this, in conjunction with @CNSaveRequest.transactionAuthor,@              to suppress processing of changes you already know about.
--
-- ObjC selector: @- excludedTransactionAuthors@
excludedTransactionAuthors :: IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest => cnChangeHistoryFetchRequest -> IO (Id NSArray)
excludedTransactionAuthors cnChangeHistoryFetchRequest =
  sendMessage cnChangeHistoryFetchRequest excludedTransactionAuthorsSelector

-- | Exclude changes made by certain authors.
--
-- If set, transactions made by the specified authors will be excluded              from the results. Use this, in conjunction with @CNSaveRequest.transactionAuthor,@              to suppress processing of changes you already know about.
--
-- ObjC selector: @- setExcludedTransactionAuthors:@
setExcludedTransactionAuthors :: (IsCNChangeHistoryFetchRequest cnChangeHistoryFetchRequest, IsNSArray value) => cnChangeHistoryFetchRequest -> value -> IO ()
setExcludedTransactionAuthors cnChangeHistoryFetchRequest value =
  sendMessage cnChangeHistoryFetchRequest setExcludedTransactionAuthorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startingToken@
startingTokenSelector :: Selector '[] (Id NSData)
startingTokenSelector = mkSelector "startingToken"

-- | @Selector@ for @setStartingToken:@
setStartingTokenSelector :: Selector '[Id NSData] ()
setStartingTokenSelector = mkSelector "setStartingToken:"

-- | @Selector@ for @additionalContactKeyDescriptors@
additionalContactKeyDescriptorsSelector :: Selector '[] (Id NSArray)
additionalContactKeyDescriptorsSelector = mkSelector "additionalContactKeyDescriptors"

-- | @Selector@ for @setAdditionalContactKeyDescriptors:@
setAdditionalContactKeyDescriptorsSelector :: Selector '[Id NSArray] ()
setAdditionalContactKeyDescriptorsSelector = mkSelector "setAdditionalContactKeyDescriptors:"

-- | @Selector@ for @shouldUnifyResults@
shouldUnifyResultsSelector :: Selector '[] Bool
shouldUnifyResultsSelector = mkSelector "shouldUnifyResults"

-- | @Selector@ for @setShouldUnifyResults:@
setShouldUnifyResultsSelector :: Selector '[Bool] ()
setShouldUnifyResultsSelector = mkSelector "setShouldUnifyResults:"

-- | @Selector@ for @mutableObjects@
mutableObjectsSelector :: Selector '[] Bool
mutableObjectsSelector = mkSelector "mutableObjects"

-- | @Selector@ for @setMutableObjects:@
setMutableObjectsSelector :: Selector '[Bool] ()
setMutableObjectsSelector = mkSelector "setMutableObjects:"

-- | @Selector@ for @includeGroupChanges@
includeGroupChangesSelector :: Selector '[] Bool
includeGroupChangesSelector = mkSelector "includeGroupChanges"

-- | @Selector@ for @setIncludeGroupChanges:@
setIncludeGroupChangesSelector :: Selector '[Bool] ()
setIncludeGroupChangesSelector = mkSelector "setIncludeGroupChanges:"

-- | @Selector@ for @excludedTransactionAuthors@
excludedTransactionAuthorsSelector :: Selector '[] (Id NSArray)
excludedTransactionAuthorsSelector = mkSelector "excludedTransactionAuthors"

-- | @Selector@ for @setExcludedTransactionAuthors:@
setExcludedTransactionAuthorsSelector :: Selector '[Id NSArray] ()
setExcludedTransactionAuthorsSelector = mkSelector "setExcludedTransactionAuthors:"

