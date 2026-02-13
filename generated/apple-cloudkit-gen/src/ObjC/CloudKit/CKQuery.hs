{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKQuery
--
-- Only AND compound predicates are allowed.
--
-- Key names must begin with either an upper or lower case character ([a-zA-Z]) and may be followed by characters, numbers, or underscores ([0-9a-zA-Z_]). Keypaths may only resolve to the currently evaluated object, so the '.' character is not allowed in key names.
--
-- A limited subset of classes are allowed as predicate arguments:  - NSString  - NSDate  - NSData  - NSNumber  - NSArray  - CKReference  - CKRecord  - CLLocation
--
-- Any other class as an argument will result in an error when executing the query.
--
-- Generated bindings for @CKQuery@.
module ObjC.CloudKit.CKQuery
  ( CKQuery
  , IsCKQuery(..)
  , init_
  , new
  , initWithCoder
  , initWithRecordType_predicate
  , recordType
  , predicate
  , sortDescriptors
  , setSortDescriptors
  , initSelector
  , initWithCoderSelector
  , initWithRecordType_predicateSelector
  , newSelector
  , predicateSelector
  , recordTypeSelector
  , setSortDescriptorsSelector
  , sortDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKQuery ckQuery => ckQuery -> IO (Id CKQuery)
init_ ckQuery =
  sendOwnedMessage ckQuery initSelector

-- | @+ new@
new :: IO (Id CKQuery)
new  =
  do
    cls' <- getRequiredClass "CKQuery"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithCoder:@
initWithCoder :: (IsCKQuery ckQuery, IsNSCoder aDecoder) => ckQuery -> aDecoder -> IO (Id CKQuery)
initWithCoder ckQuery aDecoder =
  sendOwnedMessage ckQuery initWithCoderSelector (toNSCoder aDecoder)

-- | Use
--
-- [NSPredicate predicateWithValue:YES] / NSPredicate(value: true)
--
-- if you want to query for all records of a given type.
--
-- ObjC selector: @- initWithRecordType:predicate:@
initWithRecordType_predicate :: (IsCKQuery ckQuery, IsNSString recordType, IsNSPredicate predicate) => ckQuery -> recordType -> predicate -> IO (Id CKQuery)
initWithRecordType_predicate ckQuery recordType predicate =
  sendOwnedMessage ckQuery initWithRecordType_predicateSelector (toNSString recordType) (toNSPredicate predicate)

-- | @- recordType@
recordType :: IsCKQuery ckQuery => ckQuery -> IO (Id NSString)
recordType ckQuery =
  sendMessage ckQuery recordTypeSelector

-- | @- predicate@
predicate :: IsCKQuery ckQuery => ckQuery -> IO (Id NSPredicate)
predicate ckQuery =
  sendMessage ckQuery predicateSelector

-- | @- sortDescriptors@
sortDescriptors :: IsCKQuery ckQuery => ckQuery -> IO (Id NSArray)
sortDescriptors ckQuery =
  sendMessage ckQuery sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsCKQuery ckQuery, IsNSArray value) => ckQuery -> value -> IO ()
setSortDescriptors ckQuery value =
  sendMessage ckQuery setSortDescriptorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKQuery)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKQuery)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CKQuery)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithRecordType:predicate:@
initWithRecordType_predicateSelector :: Selector '[Id NSString, Id NSPredicate] (Id CKQuery)
initWithRecordType_predicateSelector = mkSelector "initWithRecordType:predicate:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector '[Id NSArray] ()
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

