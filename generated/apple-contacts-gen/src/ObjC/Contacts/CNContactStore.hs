{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides methods to fetch and save contacts.
--
-- The CNContactStore is a thread safe class that can fetch and save contacts, fetch and save groups, and fetch containers.
--
-- Note: Some good practices are: 1) Only fetch contact properties that will be used. 2) When fetching all contacts and caching the results, first fetch all contact identifiers only. Then fetch batches of detailed contacts by identifiers as you need them. 3) To aggregate several contact fetches collect a set of unique contact identifiers from the fetches. Then fetch batches of detailed contacts by identifiers. 4) When CNContactStoreDidChangeNotification is posted, if you cache any fetched contacts/groups/containers then they must be refetched and the old cached objects released.
--
-- Generated bindings for @CNContactStore@.
module ObjC.Contacts.CNContactStore
  ( CNContactStore
  , IsCNContactStore(..)
  , authorizationStatusForEntityType
  , requestAccessForEntityType_completionHandler
  , unifiedContactsMatchingPredicate_keysToFetch_error
  , unifiedContactWithIdentifier_keysToFetch_error
  , unifiedMeContactWithKeysToFetch_error
  , enumeratorForContactFetchRequest_error
  , enumeratorForChangeHistoryFetchRequest_error
  , enumerateContactsWithFetchRequest_error_usingBlock
  , groupsMatchingPredicate_error
  , containersMatchingPredicate_error
  , executeSaveRequest_error
  , defaultContainerIdentifier
  , currentHistoryToken
  , authorizationStatusForEntityTypeSelector
  , containersMatchingPredicate_errorSelector
  , currentHistoryTokenSelector
  , defaultContainerIdentifierSelector
  , enumerateContactsWithFetchRequest_error_usingBlockSelector
  , enumeratorForChangeHistoryFetchRequest_errorSelector
  , enumeratorForContactFetchRequest_errorSelector
  , executeSaveRequest_errorSelector
  , groupsMatchingPredicate_errorSelector
  , requestAccessForEntityType_completionHandlerSelector
  , unifiedContactWithIdentifier_keysToFetch_errorSelector
  , unifiedContactsMatchingPredicate_keysToFetch_errorSelector
  , unifiedMeContactWithKeysToFetch_errorSelector

  -- * Enum types
  , CNAuthorizationStatus(CNAuthorizationStatus)
  , pattern CNAuthorizationStatusNotDetermined
  , pattern CNAuthorizationStatusRestricted
  , pattern CNAuthorizationStatusDenied
  , pattern CNAuthorizationStatusAuthorized
  , pattern CNAuthorizationStatusLimited
  , CNEntityType(CNEntityType)
  , pattern CNEntityTypeContacts

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Indicates the current authorization status to access contact data.
--
-- Based upon the access, the application could display or hide its UI elements that would access any Contacts API. This method is thread safe.
--
-- Returns: Returns the authorization status for the given entityType.
--
-- ObjC selector: @+ authorizationStatusForEntityType:@
authorizationStatusForEntityType :: CNEntityType -> IO CNAuthorizationStatus
authorizationStatusForEntityType entityType =
  do
    cls' <- getRequiredClass "CNContactStore"
    sendClassMessage cls' authorizationStatusForEntityTypeSelector entityType

-- | Request access to the user's contacts.
--
-- Users are able to grant or deny access to contact data on a per-application basis. To request access to contact data, call requestAccessForEntityType:completionHandler:. This will not block the application while the user is being asked to grant or deny access. The user will only be prompted the first time access is requested; any subsequent CNContactStore calls will use the existing permissions. The completion handler is called on an arbitrary queue.
--
-- Note: Recommended to use CNContactStore instance methods in this completion handler instead of the UI main thread. This method is optional when CNContactStore is used on a background thread. If it is not used in that case, CNContactStore will block if the user is asked to grant or deny access.
--
-- @entityType@ — Set to CNEntityTypeContacts.
--
-- @completionHandler@ — This block is called upon completion. If the user grants access then granted is YES and error is nil. Otherwise granted is NO with an error.
--
-- ObjC selector: @- requestAccessForEntityType:completionHandler:@
requestAccessForEntityType_completionHandler :: IsCNContactStore cnContactStore => cnContactStore -> CNEntityType -> Ptr () -> IO ()
requestAccessForEntityType_completionHandler cnContactStore entityType completionHandler =
  sendMessage cnContactStore requestAccessForEntityType_completionHandlerSelector entityType completionHandler

-- | Fetch all unified contacts matching a given predicate.
--
-- Use only predicates from CNContact+Predicates.h. Compound predicates are not supported. Due to unification the returned contacts may have a different identifier.
--
-- Note: To fetch all contacts use enumerateContactsWithFetchRequest:error:usingBlock:.
--
-- @predicate@ — The predicate to match against.
--
-- @keys@ — The properties to fetch into the returned CNContact objects. Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- @error@ — If an error occurs, contains error information.
--
-- Returns: An array of CNContact objects matching the predicate. If no matches are found, an empty array is returned. If an error occurs, nil is returned.
--
-- ObjC selector: @- unifiedContactsMatchingPredicate:keysToFetch:error:@
unifiedContactsMatchingPredicate_keysToFetch_error :: (IsCNContactStore cnContactStore, IsNSPredicate predicate, IsNSArray keys, IsNSError error_) => cnContactStore -> predicate -> keys -> error_ -> IO (Id NSArray)
unifiedContactsMatchingPredicate_keysToFetch_error cnContactStore predicate keys error_ =
  sendMessage cnContactStore unifiedContactsMatchingPredicate_keysToFetch_errorSelector (toNSPredicate predicate) (toNSArray keys) (toNSError error_)

-- | Fetch a unified contact with a given identifier.
--
-- Due to unification the returned contact may have a different identifier. To fetch a batch of contacts by identifiers use [CNContact predicateForContactsWithIdentifiers:].
--
-- @identifier@ — The identifier of the contact to fetch.
--
-- @keys@ — The properties to fetch into the returned CNContact object. Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- @error@ — If an error occurs, contains error information.
--
-- Returns: The unified contact matching or linked to the identifier. If no contact with the given identifier is found, nil is returned and error is set to CNErrorCodeRecordDoesNotExist.
--
-- ObjC selector: @- unifiedContactWithIdentifier:keysToFetch:error:@
unifiedContactWithIdentifier_keysToFetch_error :: (IsCNContactStore cnContactStore, IsNSString identifier, IsNSArray keys, IsNSError error_) => cnContactStore -> identifier -> keys -> error_ -> IO (Id CNContact)
unifiedContactWithIdentifier_keysToFetch_error cnContactStore identifier keys error_ =
  sendMessage cnContactStore unifiedContactWithIdentifier_keysToFetch_errorSelector (toNSString identifier) (toNSArray keys) (toNSError error_)

-- | Fetch the unified contact that is the "me" card.
--
-- Fetches the contact that is represented in the user interface as "My Card".
--
-- @keys@ — The properties to fetch into the returned CNContact object. Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- @error@ — If an error occurs, contains error information.
--
-- Returns: The unified contact that is the "me" card. If no "me" card is set, nil is returned.
--
-- ObjC selector: @- unifiedMeContactWithKeysToFetch:error:@
unifiedMeContactWithKeysToFetch_error :: (IsCNContactStore cnContactStore, IsNSArray keys, IsNSError error_) => cnContactStore -> keys -> error_ -> IO (Id CNContact)
unifiedMeContactWithKeysToFetch_error cnContactStore keys error_ =
  sendMessage cnContactStore unifiedMeContactWithKeysToFetch_errorSelector (toNSArray keys) (toNSError error_)

-- | Enumerate a contact fetch request.
--
-- Executes the given fetch request and returns an enumerator for the results.              This may prevent all records from being loaded into memory at once.
--
-- An exception may be thrown if an error occurs during enumeration.
--
-- @request@ — A description of the records to fetch.
--
-- @error@ — If the fetch fails, contains an @NSError@ object with more information.
--
-- Returns: An enumerator of the records matching the result, or @nil@ if there was an error.
--
-- ObjC selector: @- enumeratorForContactFetchRequest:error:@
enumeratorForContactFetchRequest_error :: (IsCNContactStore cnContactStore, IsCNContactFetchRequest request, IsNSError error_) => cnContactStore -> request -> error_ -> IO (Id CNFetchResult)
enumeratorForContactFetchRequest_error cnContactStore request error_ =
  sendMessage cnContactStore enumeratorForContactFetchRequest_errorSelector (toCNContactFetchRequest request) (toNSError error_)

-- | Enumerate a change history fetch request.
--
-- Executes the given fetch request and returns an enumerator for the results.              This may prevent all events from being loaded into memory at once.
--
-- An exception may be thrown if an error occurs during enumeration.
--
-- @request@ — A description of the events to fetch.
--
-- @error@ — If the fetch fails, contains an @NSError@ object with more information.
--
-- Returns: An enumerator of the events matching the result, or @nil@ if there was an error.
--
-- ObjC selector: @- enumeratorForChangeHistoryFetchRequest:error:@
enumeratorForChangeHistoryFetchRequest_error :: (IsCNContactStore cnContactStore, IsCNChangeHistoryFetchRequest request, IsNSError error_) => cnContactStore -> request -> error_ -> IO (Id CNFetchResult)
enumeratorForChangeHistoryFetchRequest_error cnContactStore request error_ =
  sendMessage cnContactStore enumeratorForChangeHistoryFetchRequest_errorSelector (toCNChangeHistoryFetchRequest request) (toNSError error_)

-- | Enumerates all contacts matching a contact fetch request.
--
-- This method will wait until the enumeration is finished. If there are no results, the block is not called and YES is returned.
--
-- @fetchRequest@ — The contact fetch request that specifies the search criteria.
--
-- @error@ — If an error occurs, contains error information.
--
-- @block@ — Called for each matching contact. Set *stop to YES to stop the enumeration.
--
-- Returns: YES if successful, otherwise NO.
--
-- ObjC selector: @- enumerateContactsWithFetchRequest:error:usingBlock:@
enumerateContactsWithFetchRequest_error_usingBlock :: (IsCNContactStore cnContactStore, IsCNContactFetchRequest fetchRequest, IsNSError error_) => cnContactStore -> fetchRequest -> error_ -> Ptr () -> IO Bool
enumerateContactsWithFetchRequest_error_usingBlock cnContactStore fetchRequest error_ block =
  sendMessage cnContactStore enumerateContactsWithFetchRequest_error_usingBlockSelector (toCNContactFetchRequest fetchRequest) (toNSError error_) block

-- | Fetch all groups matching a given predicate.
--
-- Use only predicates from CNGroup+Predicates.h. Compound predicates are not supported.
--
-- @predicate@ — The predicate to match against. Set to nil to match all groups.
--
-- @error@ — If an error occurs, contains error information.
--
-- Returns: An array of CNGroup objects matching the predicate. If no matches are found, an empty array is returned. If an error occurs, nil is returned.
--
-- ObjC selector: @- groupsMatchingPredicate:error:@
groupsMatchingPredicate_error :: (IsCNContactStore cnContactStore, IsNSPredicate predicate, IsNSError error_) => cnContactStore -> predicate -> error_ -> IO (Id NSArray)
groupsMatchingPredicate_error cnContactStore predicate error_ =
  sendMessage cnContactStore groupsMatchingPredicate_errorSelector (toNSPredicate predicate) (toNSError error_)

-- | Fetch all containers matching a given predicate.
--
-- Use only predicates from CNContainer+Predicates.h. Compound predicates are not supported.
--
-- @predicate@ — The predicate to match against. Set to nil to match all containers.
--
-- @error@ — If an error occurs, contains error information.
--
-- Returns: An array of CNContainer objects matching the predicate. If no matches are found, an empty array is returned. If an error occurs, nil is returned.
--
-- ObjC selector: @- containersMatchingPredicate:error:@
containersMatchingPredicate_error :: (IsCNContactStore cnContactStore, IsNSPredicate predicate, IsNSError error_) => cnContactStore -> predicate -> error_ -> IO (Id NSArray)
containersMatchingPredicate_error cnContactStore predicate error_ =
  sendMessage cnContactStore containersMatchingPredicate_errorSelector (toNSPredicate predicate) (toNSError error_)

-- | Executes a save request.
--
-- Do not access objects when save request is executing. A save request with contacts may modify the contacts while executing. A save request only applies the changes to the objects. If there are overlapping changes with multiple, concurrent CNSaveRequests then the last saved change wins.
--
-- @saveRequest@ — Save request to execute.
--
-- @error@ — If an error occurs, contains error information.
--
-- Returns: YES if successful, otherwise NO.
--
-- ObjC selector: @- executeSaveRequest:error:@
executeSaveRequest_error :: (IsCNContactStore cnContactStore, IsCNSaveRequest saveRequest, IsNSError error_) => cnContactStore -> saveRequest -> error_ -> IO Bool
executeSaveRequest_error cnContactStore saveRequest error_ =
  sendMessage cnContactStore executeSaveRequest_errorSelector (toCNSaveRequest saveRequest) (toNSError error_)

-- | The identifier of the default container.
--
-- This identifier can be used to fetch the default container.
--
-- Returns: The identifier of the default container. If the caller lacks Contacts authorization or an error occurs, nil is returned.
--
-- ObjC selector: @- defaultContainerIdentifier@
defaultContainerIdentifier :: IsCNContactStore cnContactStore => cnContactStore -> IO (Id NSString)
defaultContainerIdentifier cnContactStore =
  sendMessage cnContactStore defaultContainerIdentifierSelector

-- | The current history token.
--
-- Retrieve the current history token. If you are fetching contacts or change history events, you should use the token on the @CNFetchResult@ instead.
--
-- ObjC selector: @- currentHistoryToken@
currentHistoryToken :: IsCNContactStore cnContactStore => cnContactStore -> IO (Id NSData)
currentHistoryToken cnContactStore =
  sendMessage cnContactStore currentHistoryTokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatusForEntityType:@
authorizationStatusForEntityTypeSelector :: Selector '[CNEntityType] CNAuthorizationStatus
authorizationStatusForEntityTypeSelector = mkSelector "authorizationStatusForEntityType:"

-- | @Selector@ for @requestAccessForEntityType:completionHandler:@
requestAccessForEntityType_completionHandlerSelector :: Selector '[CNEntityType, Ptr ()] ()
requestAccessForEntityType_completionHandlerSelector = mkSelector "requestAccessForEntityType:completionHandler:"

-- | @Selector@ for @unifiedContactsMatchingPredicate:keysToFetch:error:@
unifiedContactsMatchingPredicate_keysToFetch_errorSelector :: Selector '[Id NSPredicate, Id NSArray, Id NSError] (Id NSArray)
unifiedContactsMatchingPredicate_keysToFetch_errorSelector = mkSelector "unifiedContactsMatchingPredicate:keysToFetch:error:"

-- | @Selector@ for @unifiedContactWithIdentifier:keysToFetch:error:@
unifiedContactWithIdentifier_keysToFetch_errorSelector :: Selector '[Id NSString, Id NSArray, Id NSError] (Id CNContact)
unifiedContactWithIdentifier_keysToFetch_errorSelector = mkSelector "unifiedContactWithIdentifier:keysToFetch:error:"

-- | @Selector@ for @unifiedMeContactWithKeysToFetch:error:@
unifiedMeContactWithKeysToFetch_errorSelector :: Selector '[Id NSArray, Id NSError] (Id CNContact)
unifiedMeContactWithKeysToFetch_errorSelector = mkSelector "unifiedMeContactWithKeysToFetch:error:"

-- | @Selector@ for @enumeratorForContactFetchRequest:error:@
enumeratorForContactFetchRequest_errorSelector :: Selector '[Id CNContactFetchRequest, Id NSError] (Id CNFetchResult)
enumeratorForContactFetchRequest_errorSelector = mkSelector "enumeratorForContactFetchRequest:error:"

-- | @Selector@ for @enumeratorForChangeHistoryFetchRequest:error:@
enumeratorForChangeHistoryFetchRequest_errorSelector :: Selector '[Id CNChangeHistoryFetchRequest, Id NSError] (Id CNFetchResult)
enumeratorForChangeHistoryFetchRequest_errorSelector = mkSelector "enumeratorForChangeHistoryFetchRequest:error:"

-- | @Selector@ for @enumerateContactsWithFetchRequest:error:usingBlock:@
enumerateContactsWithFetchRequest_error_usingBlockSelector :: Selector '[Id CNContactFetchRequest, Id NSError, Ptr ()] Bool
enumerateContactsWithFetchRequest_error_usingBlockSelector = mkSelector "enumerateContactsWithFetchRequest:error:usingBlock:"

-- | @Selector@ for @groupsMatchingPredicate:error:@
groupsMatchingPredicate_errorSelector :: Selector '[Id NSPredicate, Id NSError] (Id NSArray)
groupsMatchingPredicate_errorSelector = mkSelector "groupsMatchingPredicate:error:"

-- | @Selector@ for @containersMatchingPredicate:error:@
containersMatchingPredicate_errorSelector :: Selector '[Id NSPredicate, Id NSError] (Id NSArray)
containersMatchingPredicate_errorSelector = mkSelector "containersMatchingPredicate:error:"

-- | @Selector@ for @executeSaveRequest:error:@
executeSaveRequest_errorSelector :: Selector '[Id CNSaveRequest, Id NSError] Bool
executeSaveRequest_errorSelector = mkSelector "executeSaveRequest:error:"

-- | @Selector@ for @defaultContainerIdentifier@
defaultContainerIdentifierSelector :: Selector '[] (Id NSString)
defaultContainerIdentifierSelector = mkSelector "defaultContainerIdentifier"

-- | @Selector@ for @currentHistoryToken@
currentHistoryTokenSelector :: Selector '[] (Id NSData)
currentHistoryTokenSelector = mkSelector "currentHistoryToken"

