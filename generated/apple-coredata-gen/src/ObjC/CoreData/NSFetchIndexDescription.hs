{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFetchIndexDescription@.
module ObjC.CoreData.NSFetchIndexDescription
  ( NSFetchIndexDescription
  , IsNSFetchIndexDescription(..)
  , initWithName_elements
  , name
  , setName
  , elements
  , setElements
  , entity
  , partialIndexPredicate
  , setPartialIndexPredicate
  , elementsSelector
  , entitySelector
  , initWithName_elementsSelector
  , nameSelector
  , partialIndexPredicateSelector
  , setElementsSelector
  , setNameSelector
  , setPartialIndexPredicateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:elements:@
initWithName_elements :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSString name, IsNSArray elements) => nsFetchIndexDescription -> name -> elements -> IO (Id NSFetchIndexDescription)
initWithName_elements nsFetchIndexDescription name elements =
  sendOwnedMessage nsFetchIndexDescription initWithName_elementsSelector (toNSString name) (toNSArray elements)

-- | @- name@
name :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSString)
name nsFetchIndexDescription =
  sendMessage nsFetchIndexDescription nameSelector

-- | @- setName:@
setName :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSString value) => nsFetchIndexDescription -> value -> IO ()
setName nsFetchIndexDescription value =
  sendMessage nsFetchIndexDescription setNameSelector (toNSString value)

-- | @- elements@
elements :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSArray)
elements nsFetchIndexDescription =
  sendMessage nsFetchIndexDescription elementsSelector

-- | @- setElements:@
setElements :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSArray value) => nsFetchIndexDescription -> value -> IO ()
setElements nsFetchIndexDescription value =
  sendMessage nsFetchIndexDescription setElementsSelector (toNSArray value)

-- | @- entity@
entity :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSEntityDescription)
entity nsFetchIndexDescription =
  sendMessage nsFetchIndexDescription entitySelector

-- | @- partialIndexPredicate@
partialIndexPredicate :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSPredicate)
partialIndexPredicate nsFetchIndexDescription =
  sendMessage nsFetchIndexDescription partialIndexPredicateSelector

-- | @- setPartialIndexPredicate:@
setPartialIndexPredicate :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSPredicate value) => nsFetchIndexDescription -> value -> IO ()
setPartialIndexPredicate nsFetchIndexDescription value =
  sendMessage nsFetchIndexDescription setPartialIndexPredicateSelector (toNSPredicate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:elements:@
initWithName_elementsSelector :: Selector '[Id NSString, Id NSArray] (Id NSFetchIndexDescription)
initWithName_elementsSelector = mkSelector "initWithName:elements:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id NSArray)
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector '[Id NSArray] ()
setElementsSelector = mkSelector "setElements:"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @partialIndexPredicate@
partialIndexPredicateSelector :: Selector '[] (Id NSPredicate)
partialIndexPredicateSelector = mkSelector "partialIndexPredicate"

-- | @Selector@ for @setPartialIndexPredicate:@
setPartialIndexPredicateSelector :: Selector '[Id NSPredicate] ()
setPartialIndexPredicateSelector = mkSelector "setPartialIndexPredicate:"

