{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addObserver_forKeyPath_options_contextSelector
  , allObjectsSelector
  , anyObjectSelector
  , containsObjectSelector
  , countSelector
  , descriptionSelector
  , descriptionWithLocaleSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , filteredSetUsingPredicateSelector
  , initSelector
  , initWithArraySelector
  , initWithCoderSelector
  , initWithObjectsSelector
  , initWithObjects_countSelector
  , initWithSetSelector
  , initWithSet_copyItemsSelector
  , intersectsSetSelector
  , isEqualToSetSelector
  , isSubsetOfSetSelector
  , makeObjectsPerformSelectorSelector
  , makeObjectsPerformSelector_withObjectSelector
  , memberSelector
  , objectEnumeratorSelector
  , objectsPassingTestSelector
  , objectsWithOptions_passingTestSelector
  , removeObserver_forKeyPathSelector
  , removeObserver_forKeyPath_contextSelector
  , setByAddingObjectSelector
  , setByAddingObjectsFromArraySelector
  , setByAddingObjectsFromSetSelector
  , setSelector
  , setValue_forKeySelector
  , setWithArraySelector
  , setWithObjectSelector
  , setWithObjectsSelector
  , setWithObjects_countSelector
  , setWithSetSelector
  , sortedArrayUsingDescriptorsSelector
  , valueForKeySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- member:@
member :: IsNSSet nsSet => nsSet -> RawId -> IO RawId
member nsSet object =
  sendMessage nsSet memberSelector object

-- | @- objectEnumerator@
objectEnumerator :: IsNSSet nsSet => nsSet -> IO (Id NSEnumerator)
objectEnumerator nsSet =
  sendMessage nsSet objectEnumeratorSelector

-- | @- init@
init_ :: IsNSSet nsSet => nsSet -> IO (Id NSSet)
init_ nsSet =
  sendOwnedMessage nsSet initSelector

-- | @- initWithObjects:count:@
initWithObjects_count :: IsNSSet nsSet => nsSet -> RawId -> CULong -> IO (Id NSSet)
initWithObjects_count nsSet objects cnt =
  sendOwnedMessage nsSet initWithObjects_countSelector objects cnt

-- | @- initWithCoder:@
initWithCoder :: (IsNSSet nsSet, IsNSCoder coder) => nsSet -> coder -> IO (Id NSSet)
initWithCoder nsSet coder =
  sendOwnedMessage nsSet initWithCoderSelector (toNSCoder coder)

-- | @- filteredSetUsingPredicate:@
filteredSetUsingPredicate :: (IsNSSet nsSet, IsNSPredicate predicate) => nsSet -> predicate -> IO (Id NSSet)
filteredSetUsingPredicate nsSet predicate =
  sendMessage nsSet filteredSetUsingPredicateSelector (toNSPredicate predicate)

-- | @- sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptors :: (IsNSSet nsSet, IsNSArray sortDescriptors) => nsSet -> sortDescriptors -> IO (Id NSArray)
sortedArrayUsingDescriptors nsSet sortDescriptors =
  sendMessage nsSet sortedArrayUsingDescriptorsSelector (toNSArray sortDescriptors)

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSSet nsSet, IsNSObject observer, IsNSString keyPath) => nsSet -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsSet observer keyPath options context =
  sendMessage nsSet addObserver_forKeyPath_options_contextSelector (toNSObject observer) (toNSString keyPath) options context

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSSet nsSet, IsNSObject observer, IsNSString keyPath) => nsSet -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsSet observer keyPath context =
  sendMessage nsSet removeObserver_forKeyPath_contextSelector (toNSObject observer) (toNSString keyPath) context

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSSet nsSet, IsNSObject observer, IsNSString keyPath) => nsSet -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsSet observer keyPath =
  sendMessage nsSet removeObserver_forKeyPathSelector (toNSObject observer) (toNSString keyPath)

-- | @- valueForKey:@
valueForKey :: (IsNSSet nsSet, IsNSString key) => nsSet -> key -> IO RawId
valueForKey nsSet key =
  sendMessage nsSet valueForKeySelector (toNSString key)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSSet nsSet, IsNSString key) => nsSet -> RawId -> key -> IO ()
setValue_forKey nsSet value key =
  sendMessage nsSet setValue_forKeySelector value (toNSString key)

-- | @+ set@
set :: IO (Id NSSet)
set  =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setSelector

-- | @+ setWithObject:@
setWithObject :: RawId -> IO (Id NSSet)
setWithObject object =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setWithObjectSelector object

-- | @+ setWithObjects:count:@
setWithObjects_count :: RawId -> CULong -> IO (Id NSSet)
setWithObjects_count objects cnt =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setWithObjects_countSelector objects cnt

-- | @+ setWithObjects:@
setWithObjects :: RawId -> IO (Id NSSet)
setWithObjects firstObj =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setWithObjectsSelector firstObj

-- | @+ setWithSet:@
setWithSet :: IsNSSet set => set -> IO (Id NSSet)
setWithSet set =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setWithSetSelector (toNSSet set)

-- | @+ setWithArray:@
setWithArray :: IsNSArray array => array -> IO (Id NSSet)
setWithArray array =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setWithArraySelector (toNSArray array)

-- | @- initWithObjects:@
initWithObjects :: IsNSSet nsSet => nsSet -> RawId -> IO (Id NSSet)
initWithObjects nsSet firstObj =
  sendOwnedMessage nsSet initWithObjectsSelector firstObj

-- | @- initWithSet:@
initWithSet :: (IsNSSet nsSet, IsNSSet set) => nsSet -> set -> IO (Id NSSet)
initWithSet nsSet set =
  sendOwnedMessage nsSet initWithSetSelector (toNSSet set)

-- | @- initWithSet:copyItems:@
initWithSet_copyItems :: (IsNSSet nsSet, IsNSSet set) => nsSet -> set -> Bool -> IO (Id NSSet)
initWithSet_copyItems nsSet set flag =
  sendOwnedMessage nsSet initWithSet_copyItemsSelector (toNSSet set) flag

-- | @- initWithArray:@
initWithArray :: (IsNSSet nsSet, IsNSArray array) => nsSet -> array -> IO (Id NSSet)
initWithArray nsSet array =
  sendOwnedMessage nsSet initWithArraySelector (toNSArray array)

-- | @- anyObject@
anyObject :: IsNSSet nsSet => nsSet -> IO RawId
anyObject nsSet =
  sendMessage nsSet anyObjectSelector

-- | @- containsObject:@
containsObject :: IsNSSet nsSet => nsSet -> RawId -> IO Bool
containsObject nsSet anObject =
  sendMessage nsSet containsObjectSelector anObject

-- | @- descriptionWithLocale:@
descriptionWithLocale :: IsNSSet nsSet => nsSet -> RawId -> IO (Id NSString)
descriptionWithLocale nsSet locale =
  sendMessage nsSet descriptionWithLocaleSelector locale

-- | @- intersectsSet:@
intersectsSet :: (IsNSSet nsSet, IsNSSet otherSet) => nsSet -> otherSet -> IO Bool
intersectsSet nsSet otherSet =
  sendMessage nsSet intersectsSetSelector (toNSSet otherSet)

-- | @- isEqualToSet:@
isEqualToSet :: (IsNSSet nsSet, IsNSSet otherSet) => nsSet -> otherSet -> IO Bool
isEqualToSet nsSet otherSet =
  sendMessage nsSet isEqualToSetSelector (toNSSet otherSet)

-- | @- isSubsetOfSet:@
isSubsetOfSet :: (IsNSSet nsSet, IsNSSet otherSet) => nsSet -> otherSet -> IO Bool
isSubsetOfSet nsSet otherSet =
  sendMessage nsSet isSubsetOfSetSelector (toNSSet otherSet)

-- | @- makeObjectsPerformSelector:@
makeObjectsPerformSelector :: IsNSSet nsSet => nsSet -> Sel -> IO ()
makeObjectsPerformSelector nsSet aSelector =
  sendMessage nsSet makeObjectsPerformSelectorSelector aSelector

-- | @- makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObject :: IsNSSet nsSet => nsSet -> Sel -> RawId -> IO ()
makeObjectsPerformSelector_withObject nsSet aSelector argument =
  sendMessage nsSet makeObjectsPerformSelector_withObjectSelector aSelector argument

-- | @- setByAddingObject:@
setByAddingObject :: IsNSSet nsSet => nsSet -> RawId -> IO (Id NSSet)
setByAddingObject nsSet anObject =
  sendMessage nsSet setByAddingObjectSelector anObject

-- | @- setByAddingObjectsFromSet:@
setByAddingObjectsFromSet :: (IsNSSet nsSet, IsNSSet other) => nsSet -> other -> IO (Id NSSet)
setByAddingObjectsFromSet nsSet other =
  sendMessage nsSet setByAddingObjectsFromSetSelector (toNSSet other)

-- | @- setByAddingObjectsFromArray:@
setByAddingObjectsFromArray :: (IsNSSet nsSet, IsNSArray other) => nsSet -> other -> IO (Id NSSet)
setByAddingObjectsFromArray nsSet other =
  sendMessage nsSet setByAddingObjectsFromArraySelector (toNSArray other)

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsNSSet nsSet => nsSet -> Ptr () -> IO ()
enumerateObjectsUsingBlock nsSet block =
  sendMessage nsSet enumerateObjectsUsingBlockSelector block

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsNSSet nsSet => nsSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock nsSet opts block =
  sendMessage nsSet enumerateObjectsWithOptions_usingBlockSelector opts block

-- | @- objectsPassingTest:@
objectsPassingTest :: IsNSSet nsSet => nsSet -> Ptr () -> IO (Id NSSet)
objectsPassingTest nsSet predicate =
  sendMessage nsSet objectsPassingTestSelector predicate

-- | @- objectsWithOptions:passingTest:@
objectsWithOptions_passingTest :: IsNSSet nsSet => nsSet -> NSEnumerationOptions -> Ptr () -> IO (Id NSSet)
objectsWithOptions_passingTest nsSet opts predicate =
  sendMessage nsSet objectsWithOptions_passingTestSelector opts predicate

-- | @- count@
count :: IsNSSet nsSet => nsSet -> IO CULong
count nsSet =
  sendMessage nsSet countSelector

-- | @- allObjects@
allObjects :: IsNSSet nsSet => nsSet -> IO (Id NSArray)
allObjects nsSet =
  sendMessage nsSet allObjectsSelector

-- | @- description@
description :: IsNSSet nsSet => nsSet -> IO (Id NSString)
description nsSet =
  sendMessage nsSet descriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member:@
memberSelector :: Selector '[RawId] RawId
memberSelector = mkSelector "member:"

-- | @Selector@ for @objectEnumerator@
objectEnumeratorSelector :: Selector '[] (Id NSEnumerator)
objectEnumeratorSelector = mkSelector "objectEnumerator"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSet)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithObjects:count:@
initWithObjects_countSelector :: Selector '[RawId, CULong] (Id NSSet)
initWithObjects_countSelector = mkSelector "initWithObjects:count:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSSet)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @filteredSetUsingPredicate:@
filteredSetUsingPredicateSelector :: Selector '[Id NSPredicate] (Id NSSet)
filteredSetUsingPredicateSelector = mkSelector "filteredSetUsingPredicate:"

-- | @Selector@ for @sortedArrayUsingDescriptors:@
sortedArrayUsingDescriptorsSelector :: Selector '[Id NSArray] (Id NSArray)
sortedArrayUsingDescriptorsSelector = mkSelector "sortedArrayUsingDescriptors:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector '[Id NSObject, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_contextSelector :: Selector '[Id NSObject, Id NSString, Ptr ()] ()
removeObserver_forKeyPath_contextSelector = mkSelector "removeObserver:forKeyPath:context:"

-- | @Selector@ for @removeObserver:forKeyPath:@
removeObserver_forKeyPathSelector :: Selector '[Id NSObject, Id NSString] ()
removeObserver_forKeyPathSelector = mkSelector "removeObserver:forKeyPath:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @set@
setSelector :: Selector '[] (Id NSSet)
setSelector = mkSelector "set"

-- | @Selector@ for @setWithObject:@
setWithObjectSelector :: Selector '[RawId] (Id NSSet)
setWithObjectSelector = mkSelector "setWithObject:"

-- | @Selector@ for @setWithObjects:count:@
setWithObjects_countSelector :: Selector '[RawId, CULong] (Id NSSet)
setWithObjects_countSelector = mkSelector "setWithObjects:count:"

-- | @Selector@ for @setWithObjects:@
setWithObjectsSelector :: Selector '[RawId] (Id NSSet)
setWithObjectsSelector = mkSelector "setWithObjects:"

-- | @Selector@ for @setWithSet:@
setWithSetSelector :: Selector '[Id NSSet] (Id NSSet)
setWithSetSelector = mkSelector "setWithSet:"

-- | @Selector@ for @setWithArray:@
setWithArraySelector :: Selector '[Id NSArray] (Id NSSet)
setWithArraySelector = mkSelector "setWithArray:"

-- | @Selector@ for @initWithObjects:@
initWithObjectsSelector :: Selector '[RawId] (Id NSSet)
initWithObjectsSelector = mkSelector "initWithObjects:"

-- | @Selector@ for @initWithSet:@
initWithSetSelector :: Selector '[Id NSSet] (Id NSSet)
initWithSetSelector = mkSelector "initWithSet:"

-- | @Selector@ for @initWithSet:copyItems:@
initWithSet_copyItemsSelector :: Selector '[Id NSSet, Bool] (Id NSSet)
initWithSet_copyItemsSelector = mkSelector "initWithSet:copyItems:"

-- | @Selector@ for @initWithArray:@
initWithArraySelector :: Selector '[Id NSArray] (Id NSSet)
initWithArraySelector = mkSelector "initWithArray:"

-- | @Selector@ for @anyObject@
anyObjectSelector :: Selector '[] RawId
anyObjectSelector = mkSelector "anyObject"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector '[RawId] Bool
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @descriptionWithLocale:@
descriptionWithLocaleSelector :: Selector '[RawId] (Id NSString)
descriptionWithLocaleSelector = mkSelector "descriptionWithLocale:"

-- | @Selector@ for @intersectsSet:@
intersectsSetSelector :: Selector '[Id NSSet] Bool
intersectsSetSelector = mkSelector "intersectsSet:"

-- | @Selector@ for @isEqualToSet:@
isEqualToSetSelector :: Selector '[Id NSSet] Bool
isEqualToSetSelector = mkSelector "isEqualToSet:"

-- | @Selector@ for @isSubsetOfSet:@
isSubsetOfSetSelector :: Selector '[Id NSSet] Bool
isSubsetOfSetSelector = mkSelector "isSubsetOfSet:"

-- | @Selector@ for @makeObjectsPerformSelector:@
makeObjectsPerformSelectorSelector :: Selector '[Sel] ()
makeObjectsPerformSelectorSelector = mkSelector "makeObjectsPerformSelector:"

-- | @Selector@ for @makeObjectsPerformSelector:withObject:@
makeObjectsPerformSelector_withObjectSelector :: Selector '[Sel, RawId] ()
makeObjectsPerformSelector_withObjectSelector = mkSelector "makeObjectsPerformSelector:withObject:"

-- | @Selector@ for @setByAddingObject:@
setByAddingObjectSelector :: Selector '[RawId] (Id NSSet)
setByAddingObjectSelector = mkSelector "setByAddingObject:"

-- | @Selector@ for @setByAddingObjectsFromSet:@
setByAddingObjectsFromSetSelector :: Selector '[Id NSSet] (Id NSSet)
setByAddingObjectsFromSetSelector = mkSelector "setByAddingObjectsFromSet:"

-- | @Selector@ for @setByAddingObjectsFromArray:@
setByAddingObjectsFromArraySelector :: Selector '[Id NSArray] (Id NSSet)
setByAddingObjectsFromArraySelector = mkSelector "setByAddingObjectsFromArray:"

-- | @Selector@ for @enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateObjectsUsingBlockSelector = mkSelector "enumerateObjectsUsingBlock:"

-- | @Selector@ for @enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateObjectsWithOptions_usingBlockSelector = mkSelector "enumerateObjectsWithOptions:usingBlock:"

-- | @Selector@ for @objectsPassingTest:@
objectsPassingTestSelector :: Selector '[Ptr ()] (Id NSSet)
objectsPassingTestSelector = mkSelector "objectsPassingTest:"

-- | @Selector@ for @objectsWithOptions:passingTest:@
objectsWithOptions_passingTestSelector :: Selector '[NSEnumerationOptions, Ptr ()] (Id NSSet)
objectsWithOptions_passingTestSelector = mkSelector "objectsWithOptions:passingTest:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector '[] (Id NSArray)
allObjectsSelector = mkSelector "allObjects"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

