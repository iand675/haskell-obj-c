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
  , newSelector
  , initWithCoderSelector
  , initWithRecordType_predicateSelector
  , recordTypeSelector
  , predicateSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKQuery ckQuery => ckQuery -> IO (Id CKQuery)
init_ ckQuery  =
  sendMsg ckQuery (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKQuery)
new  =
  do
    cls' <- getRequiredClass "CKQuery"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCKQuery ckQuery, IsNSCoder aDecoder) => ckQuery -> aDecoder -> IO (Id CKQuery)
initWithCoder ckQuery  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg ckQuery (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | Use
--
-- [NSPredicate predicateWithValue:YES] / NSPredicate(value: true)
--
-- if you want to query for all records of a given type.
--
-- ObjC selector: @- initWithRecordType:predicate:@
initWithRecordType_predicate :: (IsCKQuery ckQuery, IsNSString recordType, IsNSPredicate predicate) => ckQuery -> recordType -> predicate -> IO (Id CKQuery)
initWithRecordType_predicate ckQuery  recordType predicate =
withObjCPtr recordType $ \raw_recordType ->
  withObjCPtr predicate $ \raw_predicate ->
      sendMsg ckQuery (mkSelector "initWithRecordType:predicate:") (retPtr retVoid) [argPtr (castPtr raw_recordType :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordType@
recordType :: IsCKQuery ckQuery => ckQuery -> IO (Id NSString)
recordType ckQuery  =
  sendMsg ckQuery (mkSelector "recordType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- predicate@
predicate :: IsCKQuery ckQuery => ckQuery -> IO (Id NSPredicate)
predicate ckQuery  =
  sendMsg ckQuery (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sortDescriptors@
sortDescriptors :: IsCKQuery ckQuery => ckQuery -> IO (Id NSArray)
sortDescriptors ckQuery  =
  sendMsg ckQuery (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsCKQuery ckQuery, IsNSArray value) => ckQuery -> value -> IO ()
setSortDescriptors ckQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckQuery (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithRecordType:predicate:@
initWithRecordType_predicateSelector :: Selector
initWithRecordType_predicateSelector = mkSelector "initWithRecordType:predicate:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

