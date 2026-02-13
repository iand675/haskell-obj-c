{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifies the search criteria to fetch contacts.
--
-- Used with [CNContactStore enumerateContactsWithFetchRequest:error:usingBlock:]. Can combine any of these options to create a contact fetch request.
--
-- Generated bindings for @CNContactFetchRequest@.
module ObjC.Contacts.CNContactFetchRequest
  ( CNContactFetchRequest
  , IsCNContactFetchRequest(..)
  , init_
  , new
  , initWithKeysToFetch
  , predicate
  , setPredicate
  , keysToFetch
  , setKeysToFetch
  , mutableObjects
  , setMutableObjects
  , unifyResults
  , setUnifyResults
  , sortOrder
  , setSortOrder
  , initSelector
  , initWithKeysToFetchSelector
  , keysToFetchSelector
  , mutableObjectsSelector
  , newSelector
  , predicateSelector
  , setKeysToFetchSelector
  , setMutableObjectsSelector
  , setPredicateSelector
  , setSortOrderSelector
  , setUnifyResultsSelector
  , sortOrderSelector
  , unifyResultsSelector

  -- * Enum types
  , CNContactSortOrder(CNContactSortOrder)
  , pattern CNContactSortOrderNone
  , pattern CNContactSortOrderUserDefault
  , pattern CNContactSortOrderGivenName
  , pattern CNContactSortOrderFamilyName

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

-- | @- init@
init_ :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO (Id CNContactFetchRequest)
init_ cnContactFetchRequest =
  sendOwnedMessage cnContactFetchRequest initSelector

-- | @+ new@
new :: IO (Id CNContactFetchRequest)
new  =
  do
    cls' <- getRequiredClass "CNContactFetchRequest"
    sendOwnedClassMessage cls' newSelector

-- | @keysToFetch@ — The properties to fetch for the returned contacts.
--
-- Only fetch the properties that will be used.
--
-- ObjC selector: @- initWithKeysToFetch:@
initWithKeysToFetch :: (IsCNContactFetchRequest cnContactFetchRequest, IsNSArray keysToFetch) => cnContactFetchRequest -> keysToFetch -> IO (Id CNContactFetchRequest)
initWithKeysToFetch cnContactFetchRequest keysToFetch =
  sendOwnedMessage cnContactFetchRequest initWithKeysToFetchSelector (toNSArray keysToFetch)

-- | The predicate to match contacts against.
--
-- Use only predicates from CNContact+Predicates.h. Compound predicates are not supported. Set to nil to match all contacts.
--
-- ObjC selector: @- predicate@
predicate :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO (Id NSPredicate)
predicate cnContactFetchRequest =
  sendMessage cnContactFetchRequest predicateSelector

-- | The predicate to match contacts against.
--
-- Use only predicates from CNContact+Predicates.h. Compound predicates are not supported. Set to nil to match all contacts.
--
-- ObjC selector: @- setPredicate:@
setPredicate :: (IsCNContactFetchRequest cnContactFetchRequest, IsNSPredicate value) => cnContactFetchRequest -> value -> IO ()
setPredicate cnContactFetchRequest value =
  sendMessage cnContactFetchRequest setPredicateSelector (toNSPredicate value)

-- | The properties to fetch in the returned contacts.
--
-- Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- ObjC selector: @- keysToFetch@
keysToFetch :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO (Id NSArray)
keysToFetch cnContactFetchRequest =
  sendMessage cnContactFetchRequest keysToFetchSelector

-- | The properties to fetch in the returned contacts.
--
-- Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- ObjC selector: @- setKeysToFetch:@
setKeysToFetch :: (IsCNContactFetchRequest cnContactFetchRequest, IsNSArray value) => cnContactFetchRequest -> value -> IO ()
setKeysToFetch cnContactFetchRequest value =
  sendMessage cnContactFetchRequest setKeysToFetchSelector (toNSArray value)

-- | To return mutable contacts.
--
-- If YES returns CNMutableContact objects, otherwise returns CNContact objects. Default is NO.
--
-- ObjC selector: @- mutableObjects@
mutableObjects :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO Bool
mutableObjects cnContactFetchRequest =
  sendMessage cnContactFetchRequest mutableObjectsSelector

-- | To return mutable contacts.
--
-- If YES returns CNMutableContact objects, otherwise returns CNContact objects. Default is NO.
--
-- ObjC selector: @- setMutableObjects:@
setMutableObjects :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> Bool -> IO ()
setMutableObjects cnContactFetchRequest value =
  sendMessage cnContactFetchRequest setMutableObjectsSelector value

-- | To return linked contacts as unified contacts.
--
-- If YES returns unified contacts, otherwise returns individual contacts. Default is YES.
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts. If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- unifyResults@
unifyResults :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO Bool
unifyResults cnContactFetchRequest =
  sendMessage cnContactFetchRequest unifyResultsSelector

-- | To return linked contacts as unified contacts.
--
-- If YES returns unified contacts, otherwise returns individual contacts. Default is YES.
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts. If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- setUnifyResults:@
setUnifyResults :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> Bool -> IO ()
setUnifyResults cnContactFetchRequest value =
  sendMessage cnContactFetchRequest setUnifyResultsSelector value

-- | To return contacts in a specific sort order.
--
-- Default is CNContactSortOrderNone.
--
-- ObjC selector: @- sortOrder@
sortOrder :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO CNContactSortOrder
sortOrder cnContactFetchRequest =
  sendMessage cnContactFetchRequest sortOrderSelector

-- | To return contacts in a specific sort order.
--
-- Default is CNContactSortOrderNone.
--
-- ObjC selector: @- setSortOrder:@
setSortOrder :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> CNContactSortOrder -> IO ()
setSortOrder cnContactFetchRequest value =
  sendMessage cnContactFetchRequest setSortOrderSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNContactFetchRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNContactFetchRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithKeysToFetch:@
initWithKeysToFetchSelector :: Selector '[Id NSArray] (Id CNContactFetchRequest)
initWithKeysToFetchSelector = mkSelector "initWithKeysToFetch:"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector '[Id NSPredicate] ()
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @keysToFetch@
keysToFetchSelector :: Selector '[] (Id NSArray)
keysToFetchSelector = mkSelector "keysToFetch"

-- | @Selector@ for @setKeysToFetch:@
setKeysToFetchSelector :: Selector '[Id NSArray] ()
setKeysToFetchSelector = mkSelector "setKeysToFetch:"

-- | @Selector@ for @mutableObjects@
mutableObjectsSelector :: Selector '[] Bool
mutableObjectsSelector = mkSelector "mutableObjects"

-- | @Selector@ for @setMutableObjects:@
setMutableObjectsSelector :: Selector '[Bool] ()
setMutableObjectsSelector = mkSelector "setMutableObjects:"

-- | @Selector@ for @unifyResults@
unifyResultsSelector :: Selector '[] Bool
unifyResultsSelector = mkSelector "unifyResults"

-- | @Selector@ for @setUnifyResults:@
setUnifyResultsSelector :: Selector '[Bool] ()
setUnifyResultsSelector = mkSelector "setUnifyResults:"

-- | @Selector@ for @sortOrder@
sortOrderSelector :: Selector '[] CNContactSortOrder
sortOrderSelector = mkSelector "sortOrder"

-- | @Selector@ for @setSortOrder:@
setSortOrderSelector :: Selector '[CNContactSortOrder] ()
setSortOrderSelector = mkSelector "setSortOrder:"

