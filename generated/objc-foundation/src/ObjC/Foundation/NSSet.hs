{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Set	***************
--
-- Generated bindings for @NSSet@.
module ObjC.Foundation.NSSet
  ( NSSet
  , IsNSSet(..)
  , member
  , objectEnumerator
  , init_
  , initWithObjects_count
  , initWithCoder
  , filteredSetUsingPredicate
  , sortedArrayUsingDescriptors
  , addObserver_forKeyPath_options_context
  , removeObserver_forKeyPath_context
  , removeObserver_forKeyPath
  , valueForKey
  , setValue_forKey
  , set
  , setWithObject
  , setWithObjects_count
  , setWithObjects
  , setWithSet
  , setWithArray
  , initWithObjects
  , initWithSet
  , initWithSet_copyItems
  , initWithArray
  , anyObject
  , containsObject
  , descriptionWithLocale
  , intersectsSet
  , isEqualToSet
  , isSubsetOfSet
  , makeObjectsPerformSelector
  , makeObjectsPerformSelector_withObject
  , setByAddingObject
  , setByAddingObjectsFromSet
  , setByAddingObjectsFromArray
  , enumerateObjectsUsingBlock
  , enumerateObjectsWithOptions_usingBlock
  , objectsPassingTest
  , objectsWithOptions_passingTest
  , count
  , allObjects
  , description
  , memberSelector
  , objectEnumeratorSelector
  , initSelector
  , initWithObjects_countSelector
  , initWithCoderSelector
  , filteredSetUsingPredicateSelector
  , sortedArrayUsingDescriptorsSelector
  , addObserver_forKeyPath_options_contextSelector
  , removeObserver_forKeyPath_contextSelector
  , removeObserver_forKeyPathSelector
  , valueForKeySelector
  , setValue_forKeySelector
  , setSelector
  , setWithObjectSelector
  , setWithObjects_countSelector
  , setWithObjectsSelector
  , setWithSetSelector
  , setWithArraySelector
  , initWithObjectsSelector
  , initWithSetSelector
  , initWithSet_copyItemsSelector
  , initWithArraySelector
  , anyObjectSelector
  , containsObjectSelector
  , descriptionWithLocaleSelector
  , intersectsSetSelector
  , isEqualToSetSelector
  , isSubsetOfSetSelector
  , makeObjectsPerformSelectorSelector
  , makeObjectsPerformSelector_withObjectSelector
  , setByAddingObjectSelector
  , setByAddingObjectsFromSetSelector
  , setByAddingObjectsFromArraySelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , objectsPassingTestSelector
  , objectsWithOptions_passingTestSelector
  , countSelector
  , allObjectsSelector
  , descriptionSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse
  , NSKeyValueObservingOptions(NSKeyValueObservingOptions)
  , pattern NSKeyValueObservingOptionNew
  , pattern NSKeyValueObservingOptionOld
  , pattern NSKeyValueObservingOptionInitial
  , pattern NSKeyValueObservingOptionPrior

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- member:@
member :: IsNSSet nsSet => nsSet -> RawId -> IO RawId
member nsSet  object =
  fmap (RawId . castPtr) $ sendMsg nsSet (mkSelector "member:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- objectEnumerator@
objectEnumerator :: IsNSSet nsSet => nsSet -> IO (Id NSEnumerator)
objectEnumerator nsSet  =
  sendMsg nsSet (mkSelector "objectEnumerator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSSet nsSet => nsSet -> IO (Id NSSet)
init_ nsSet  =
  sendMsg nsSet (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithObjects:count:@
initWithObjects_count :: IsNSSet nsSet => nsSet -> RawId -> CULong -> IO (Id NSSet)
initWithObjects_count nsSet  objects cnt =
  sendMsg nsSet (mkSelector "initWithObjects:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral cnt)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSSet nsSet, IsNSCoder coder) => nsSet -> coder -> IO (Id NSSet)
initWithCoder nsSet  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsSet (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- filteredSetUsingPredicate:@
filteredSetUsingPredicate :: (IsNSSet nsSet, IsNSPredicate predicate) => nsSet -> predicate -> IO (Id NSSet)
filteredSetUsingPredicate nsSet  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg nsSet (mkSelector "filteredSetUsingPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptors :: (IsNSSet nsSet, IsNSArray sortDescriptors) => nsSet -> sortDescriptors -> IO (Id NSArray)
sortedArrayUsingDescriptors nsSet  sortDescriptors =
withObjCPtr sortDescriptors $ \raw_sortDescriptors ->
    sendMsg nsSet (mkSelector "sortedArrayUsingDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_sortDescriptors :: Ptr ())] >>= retainedObject . castPtr

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSSet nsSet, IsNSObject observer, IsNSString keyPath) => nsSet -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsSet  observer keyPath options context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsSet (mkSelector "addObserver:forKeyPath:options:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argCULong (coerce options), argPtr context]

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSSet nsSet, IsNSObject observer, IsNSString keyPath) => nsSet -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsSet  observer keyPath context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsSet (mkSelector "removeObserver:forKeyPath:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr context]

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSSet nsSet, IsNSObject observer, IsNSString keyPath) => nsSet -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsSet  observer keyPath =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsSet (mkSelector "removeObserver:forKeyPath:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- valueForKey:@
valueForKey :: (IsNSSet nsSet, IsNSString key) => nsSet -> key -> IO RawId
valueForKey nsSet  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsSet (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSSet nsSet, IsNSString key) => nsSet -> RawId -> key -> IO ()
setValue_forKey nsSet  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsSet (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @+ set@
set :: IO (Id NSSet)
set  =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMsg cls' (mkSelector "set") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setWithObject:@
setWithObject :: RawId -> IO (Id NSSet)
setWithObject object =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMsg cls' (mkSelector "setWithObject:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setWithObjects:count:@
setWithObjects_count :: RawId -> CULong -> IO (Id NSSet)
setWithObjects_count objects cnt =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMsg cls' (mkSelector "setWithObjects:count:") (retPtr retVoid) [argPtr (castPtr (unRawId objects) :: Ptr ()), argCULong (fromIntegral cnt)] >>= retainedObject . castPtr

-- | @+ setWithObjects:@
setWithObjects :: RawId -> IO (Id NSSet)
setWithObjects firstObj =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMsg cls' (mkSelector "setWithObjects:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObj) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setWithSet:@
setWithSet :: IsNSSet set => set -> IO (Id NSSet)
setWithSet set =
  do
    cls' <- getRequiredClass "NSSet"
    withObjCPtr set $ \raw_set ->
      sendClassMsg cls' (mkSelector "setWithSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setWithArray:@
setWithArray :: IsNSArray array => array -> IO (Id NSSet)
setWithArray array =
  do
    cls' <- getRequiredClass "NSSet"
    withObjCPtr array $ \raw_array ->
      sendClassMsg cls' (mkSelector "setWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithObjects:@
initWithObjects :: IsNSSet nsSet => nsSet -> RawId -> IO (Id NSSet)
initWithObjects nsSet  firstObj =
  sendMsg nsSet (mkSelector "initWithObjects:") (retPtr retVoid) [argPtr (castPtr (unRawId firstObj) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSet:@
initWithSet :: (IsNSSet nsSet, IsNSSet set) => nsSet -> set -> IO (Id NSSet)
initWithSet nsSet  set =
withObjCPtr set $ \raw_set ->
    sendMsg nsSet (mkSelector "initWithSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSet:copyItems:@
initWithSet_copyItems :: (IsNSSet nsSet, IsNSSet set) => nsSet -> set -> Bool -> IO (Id NSSet)
initWithSet_copyItems nsSet  set flag =
withObjCPtr set $ \raw_set ->
    sendMsg nsSet (mkSelector "initWithSet:copyItems:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ()), argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithArray:@
initWithArray :: (IsNSSet nsSet, IsNSArray array) => nsSet -> array -> IO (Id NSSet)
initWithArray nsSet  array =
withObjCPtr array $ \raw_array ->
    sendMsg nsSet (mkSelector "initWithArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= ownedObject . castPtr

-- | @- anyObject@
anyObject :: IsNSSet nsSet => nsSet -> IO RawId
anyObject nsSet  =
  fmap (RawId . castPtr) $ sendMsg nsSet (mkSelector "anyObject") (retPtr retVoid) []

-- | @- containsObject:@
containsObject :: IsNSSet nsSet => nsSet -> RawId -> IO Bool
containsObject nsSet  anObject =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSet (mkSelector "containsObject:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSSet nsSet => nsSet -> RawId -> IO (Id NSString)
descriptionWithLocale nsSet  locale =
  sendMsg nsSet (mkSelector "descriptionWithLocale:") (retPtr retVoid) [argPtr (castPtr (unRawId locale) :: Ptr ())] >>= retainedObject . castPtr

-- | @- intersectsSet:@
intersectsSet :: (IsNSSet nsSet, IsNSSet otherSet) => nsSet -> otherSet -> IO Bool
intersectsSet nsSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSet (mkSelector "intersectsSet:") retCULong [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- isEqualToSet:@
isEqualToSet :: (IsNSSet nsSet, IsNSSet otherSet) => nsSet -> otherSet -> IO Bool
isEqualToSet nsSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSet (mkSelector "isEqualToSet:") retCULong [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- isSubsetOfSet:@
isSubsetOfSet :: (IsNSSet nsSet, IsNSSet otherSet) => nsSet -> otherSet -> IO Bool
isSubsetOfSet nsSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSet (mkSelector "isSubsetOfSet:") retCULong [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- makeObjectsPerformSelector:@
makeObjectsPerformSelector :: IsNSSet nsSet => nsSet -> Selector -> IO ()
makeObjectsPerformSelector nsSet  aSelector =
  sendMsg nsSet (mkSelector "makeObjectsPerformSelector:") retVoid [argPtr (unSelector aSelector)]

-- | @- makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObject :: IsNSSet nsSet => nsSet -> Selector -> RawId -> IO ()
makeObjectsPerformSelector_withObject nsSet  aSelector argument =
  sendMsg nsSet (mkSelector "makeObjectsPerformSelector:withObject:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId argument) :: Ptr ())]

-- | @- setByAddingObject:@
setByAddingObject :: IsNSSet nsSet => nsSet -> RawId -> IO (Id NSSet)
setByAddingObject nsSet  anObject =
  sendMsg nsSet (mkSelector "setByAddingObject:") (retPtr retVoid) [argPtr (castPtr (unRawId anObject) :: Ptr ())] >>= retainedObject . castPtr

-- | @- setByAddingObjectsFromSet:@
setByAddingObjectsFromSet :: (IsNSSet nsSet, IsNSSet other) => nsSet -> other -> IO (Id NSSet)
setByAddingObjectsFromSet nsSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsSet (mkSelector "setByAddingObjectsFromSet:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ())] >>= retainedObject . castPtr

-- | @- setByAddingObjectsFromArray:@
setByAddingObjectsFromArray :: (IsNSSet nsSet, IsNSArray other) => nsSet -> other -> IO (Id NSSet)
setByAddingObjectsFromArray nsSet  other =
withObjCPtr other $ \raw_other ->
    sendMsg nsSet (mkSelector "setByAddingObjectsFromArray:") (retPtr retVoid) [argPtr (castPtr raw_other :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsNSSet nsSet => nsSet -> Ptr () -> IO ()
enumerateObjectsUsingBlock nsSet  block =
  sendMsg nsSet (mkSelector "enumerateObjectsUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsNSSet nsSet => nsSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock nsSet  opts block =
  sendMsg nsSet (mkSelector "enumerateObjectsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- objectsPassingTest:@
objectsPassingTest :: IsNSSet nsSet => nsSet -> Ptr () -> IO (Id NSSet)
objectsPassingTest nsSet  predicate =
  sendMsg nsSet (mkSelector "objectsPassingTest:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectsWithOptions:passingTest:@
objectsWithOptions_passingTest :: IsNSSet nsSet => nsSet -> NSEnumerationOptions -> Ptr () -> IO (Id NSSet)
objectsWithOptions_passingTest nsSet  opts predicate =
  sendMsg nsSet (mkSelector "objectsWithOptions:passingTest:") (retPtr retVoid) [argCULong (coerce opts), argPtr (castPtr predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- count@
count :: IsNSSet nsSet => nsSet -> IO CULong
count nsSet  =
  sendMsg nsSet (mkSelector "count") retCULong []

-- | @- allObjects@
allObjects :: IsNSSet nsSet => nsSet -> IO (Id NSArray)
allObjects nsSet  =
  sendMsg nsSet (mkSelector "allObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- description@
description :: IsNSSet nsSet => nsSet -> IO (Id NSString)
description nsSet  =
  sendMsg nsSet (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member:@
memberSelector :: Selector
memberSelector = mkSelector "member:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:count:@
initWithObjects_countSelector :: Selector
initWithObjects_countSelector = mkSelector "initWithObjects:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filteredSetUsingPredicate:@
filteredSetUsingPredicateSelector :: Selector
filteredSetUsingPredicateSelector = mkSelector "filteredSetUsingPredicate:"

-- | @Selector@ for @sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptorsSelector :: Selector
sortedArrayUsingDescriptorsSelector = mkSelector "sortedArrayUsingDescriptors:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_contextSelector :: Selector
removeObserver_forKeyPath_contextSelector = mkSelector "removeObserver:forKeyPath:context:"

-- | @Selector@ for @removeObserver:forKeyPath:@
removeObserver_forKeyPathSelector :: Selector
removeObserver_forKeyPathSelector = mkSelector "removeObserver:forKeyPath:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @setWithObject:@
setWithObjectSelector :: Selector
setWithObjectSelector = mkSelector "setWithObject:"

-- | @Selector@ for @setWithObjects:count:@
setWithObjects_countSelector :: Selector
setWithObjects_countSelector = mkSelector "setWithObjects:count:"

-- | @Selector@ for @setWithObjects:@
setWithObjectsSelector :: Selector
setWithObjectsSelector = mkSelector "setWithObjects:"

-- | @Selector@ for @setWithSet:@
setWithSetSelector :: Selector
setWithSetSelector = mkSelector "setWithSet:"

-- | @Selector@ for @setWithArray:@
setWithArraySelector :: Selector
setWithArraySelector = mkSelector "setWithArray:"

-- | @Selector@ for @initWithObjects:@
initWithObjectsSelector :: Selector
initWithObjectsSelector = mkSelector "initWithObjects:"

-- | @Selector@ for @initWithSet:@
initWithSetSelector :: Selector
initWithSetSelector = mkSelector "initWithSet:"

-- | @Selector@ for @initWithSet:copyItems:@
initWithSet_copyItemsSelector :: Selector
initWithSet_copyItemsSelector = mkSelector "initWithSet:copyItems:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @anyObject@
anyObjectSelector :: Selector
anyObjectSelector = mkSelector "anyObject"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @intersectsSet:@
intersectsSetSelector :: Selector
intersectsSetSelector = mkSelector "intersectsSet:"

-- | @Selector@ for @isEqualToSet:@
isEqualToSetSelector :: Selector
isEqualToSetSelector = mkSelector "isEqualToSet:"

-- | @Selector@ for @isSubsetOfSet:@
isSubsetOfSetSelector :: Selector
isSubsetOfSetSelector = mkSelector "isSubsetOfSet:"

-- | @Selector@ for @makeObjectsPerformSelector:@
makeObjectsPerformSelectorSelector :: Selector
makeObjectsPerformSelectorSelector = mkSelector "makeObjectsPerformSelector:"

-- | @Selector@ for @makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObjectSelector :: Selector
makeObjectsPerformSelector_withObjectSelector = mkSelector "makeObjectsPerformSelector:withObject:"

-- | @Selector@ for @setByAddingObject:@
setByAddingObjectSelector :: Selector
setByAddingObjectSelector = mkSelector "setByAddingObject:"

-- | @Selector@ for @setByAddingObjectsFromSet:@
setByAddingObjectsFromSetSelector :: Selector
setByAddingObjectsFromSetSelector = mkSelector "setByAddingObjectsFromSet:"

-- | @Selector@ for @setByAddingObjectsFromArray:@
setByAddingObjectsFromArraySelector :: Selector
setByAddingObjectsFromArraySelector = mkSelector "setByAddingObjectsFromArray:"

-- | @Selector@ for @enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlockSelector :: Selector
enumerateObjectsUsingBlockSelector = mkSelector "enumerateObjectsUsingBlock:"

-- | @Selector@ for @enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlockSelector :: Selector
enumerateObjectsWithOptions_usingBlockSelector = mkSelector "enumerateObjectsWithOptions:usingBlock:"

-- | @Selector@ for @objectsPassingTest:@
objectsPassingTestSelector :: Selector
objectsPassingTestSelector = mkSelector "objectsPassingTest:"

-- | @Selector@ for @objectsWithOptions:passingTest:@
objectsWithOptions_passingTestSelector :: Selector
objectsWithOptions_passingTestSelector = mkSelector "objectsWithOptions:passingTest:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector
allObjectsSelector = mkSelector "allObjects"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

