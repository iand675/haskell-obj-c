{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initWithKeysToFetchSelector
  , predicateSelector
  , setPredicateSelector
  , keysToFetchSelector
  , setKeysToFetchSelector
  , mutableObjectsSelector
  , setMutableObjectsSelector
  , unifyResultsSelector
  , setUnifyResultsSelector
  , sortOrderSelector
  , setSortOrderSelector

  -- * Enum types
  , CNContactSortOrder(CNContactSortOrder)
  , pattern CNContactSortOrderNone
  , pattern CNContactSortOrderUserDefault
  , pattern CNContactSortOrderGivenName
  , pattern CNContactSortOrderFamilyName

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
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO (Id CNContactFetchRequest)
init_ cnContactFetchRequest  =
    sendMsg cnContactFetchRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNContactFetchRequest)
new  =
  do
    cls' <- getRequiredClass "CNContactFetchRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @keysToFetch@ — The properties to fetch for the returned contacts.
--
-- Only fetch the properties that will be used.
--
-- ObjC selector: @- initWithKeysToFetch:@
initWithKeysToFetch :: (IsCNContactFetchRequest cnContactFetchRequest, IsNSArray keysToFetch) => cnContactFetchRequest -> keysToFetch -> IO (Id CNContactFetchRequest)
initWithKeysToFetch cnContactFetchRequest  keysToFetch =
  withObjCPtr keysToFetch $ \raw_keysToFetch ->
      sendMsg cnContactFetchRequest (mkSelector "initWithKeysToFetch:") (retPtr retVoid) [argPtr (castPtr raw_keysToFetch :: Ptr ())] >>= ownedObject . castPtr

-- | The predicate to match contacts against.
--
-- Use only predicates from CNContact+Predicates.h. Compound predicates are not supported. Set to nil to match all contacts.
--
-- ObjC selector: @- predicate@
predicate :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO (Id NSPredicate)
predicate cnContactFetchRequest  =
    sendMsg cnContactFetchRequest (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The predicate to match contacts against.
--
-- Use only predicates from CNContact+Predicates.h. Compound predicates are not supported. Set to nil to match all contacts.
--
-- ObjC selector: @- setPredicate:@
setPredicate :: (IsCNContactFetchRequest cnContactFetchRequest, IsNSPredicate value) => cnContactFetchRequest -> value -> IO ()
setPredicate cnContactFetchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cnContactFetchRequest (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The properties to fetch in the returned contacts.
--
-- Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- ObjC selector: @- keysToFetch@
keysToFetch :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO (Id NSArray)
keysToFetch cnContactFetchRequest  =
    sendMsg cnContactFetchRequest (mkSelector "keysToFetch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The properties to fetch in the returned contacts.
--
-- Should only fetch the properties that will be used. Can combine contact keys and contact key descriptors.
--
-- ObjC selector: @- setKeysToFetch:@
setKeysToFetch :: (IsCNContactFetchRequest cnContactFetchRequest, IsNSArray value) => cnContactFetchRequest -> value -> IO ()
setKeysToFetch cnContactFetchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cnContactFetchRequest (mkSelector "setKeysToFetch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | To return mutable contacts.
--
-- If YES returns CNMutableContact objects, otherwise returns CNContact objects. Default is NO.
--
-- ObjC selector: @- mutableObjects@
mutableObjects :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO Bool
mutableObjects cnContactFetchRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnContactFetchRequest (mkSelector "mutableObjects") retCULong []

-- | To return mutable contacts.
--
-- If YES returns CNMutableContact objects, otherwise returns CNContact objects. Default is NO.
--
-- ObjC selector: @- setMutableObjects:@
setMutableObjects :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> Bool -> IO ()
setMutableObjects cnContactFetchRequest  value =
    sendMsg cnContactFetchRequest (mkSelector "setMutableObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | To return linked contacts as unified contacts.
--
-- If YES returns unified contacts, otherwise returns individual contacts. Default is YES.
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts. If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- unifyResults@
unifyResults :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO Bool
unifyResults cnContactFetchRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnContactFetchRequest (mkSelector "unifyResults") retCULong []

-- | To return linked contacts as unified contacts.
--
-- If YES returns unified contacts, otherwise returns individual contacts. Default is YES.
--
-- Note: A unified contact is the aggregation of properties from a set of linked individual contacts. If an individual contact is not linked then the unified contact is simply that individual contact.
--
-- ObjC selector: @- setUnifyResults:@
setUnifyResults :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> Bool -> IO ()
setUnifyResults cnContactFetchRequest  value =
    sendMsg cnContactFetchRequest (mkSelector "setUnifyResults:") retVoid [argCULong (if value then 1 else 0)]

-- | To return contacts in a specific sort order.
--
-- Default is CNContactSortOrderNone.
--
-- ObjC selector: @- sortOrder@
sortOrder :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> IO CNContactSortOrder
sortOrder cnContactFetchRequest  =
    fmap (coerce :: CLong -> CNContactSortOrder) $ sendMsg cnContactFetchRequest (mkSelector "sortOrder") retCLong []

-- | To return contacts in a specific sort order.
--
-- Default is CNContactSortOrderNone.
--
-- ObjC selector: @- setSortOrder:@
setSortOrder :: IsCNContactFetchRequest cnContactFetchRequest => cnContactFetchRequest -> CNContactSortOrder -> IO ()
setSortOrder cnContactFetchRequest  value =
    sendMsg cnContactFetchRequest (mkSelector "setSortOrder:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithKeysToFetch:@
initWithKeysToFetchSelector :: Selector
initWithKeysToFetchSelector = mkSelector "initWithKeysToFetch:"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @keysToFetch@
keysToFetchSelector :: Selector
keysToFetchSelector = mkSelector "keysToFetch"

-- | @Selector@ for @setKeysToFetch:@
setKeysToFetchSelector :: Selector
setKeysToFetchSelector = mkSelector "setKeysToFetch:"

-- | @Selector@ for @mutableObjects@
mutableObjectsSelector :: Selector
mutableObjectsSelector = mkSelector "mutableObjects"

-- | @Selector@ for @setMutableObjects:@
setMutableObjectsSelector :: Selector
setMutableObjectsSelector = mkSelector "setMutableObjects:"

-- | @Selector@ for @unifyResults@
unifyResultsSelector :: Selector
unifyResultsSelector = mkSelector "unifyResults"

-- | @Selector@ for @setUnifyResults:@
setUnifyResultsSelector :: Selector
setUnifyResultsSelector = mkSelector "setUnifyResults:"

-- | @Selector@ for @sortOrder@
sortOrderSelector :: Selector
sortOrderSelector = mkSelector "sortOrder"

-- | @Selector@ for @setSortOrder:@
setSortOrderSelector :: Selector
setSortOrderSelector = mkSelector "setSortOrder:"

