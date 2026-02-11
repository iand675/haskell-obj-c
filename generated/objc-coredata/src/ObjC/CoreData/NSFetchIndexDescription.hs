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
  , initWithName_elementsSelector
  , nameSelector
  , setNameSelector
  , elementsSelector
  , setElementsSelector
  , entitySelector
  , partialIndexPredicateSelector
  , setPartialIndexPredicateSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:elements:@
initWithName_elements :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSString name, IsNSArray elements) => nsFetchIndexDescription -> name -> elements -> IO (Id NSFetchIndexDescription)
initWithName_elements nsFetchIndexDescription  name elements =
withObjCPtr name $ \raw_name ->
  withObjCPtr elements $ \raw_elements ->
      sendMsg nsFetchIndexDescription (mkSelector "initWithName:elements:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_elements :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSString)
name nsFetchIndexDescription  =
  sendMsg nsFetchIndexDescription (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSString value) => nsFetchIndexDescription -> value -> IO ()
setName nsFetchIndexDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchIndexDescription (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elements@
elements :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSArray)
elements nsFetchIndexDescription  =
  sendMsg nsFetchIndexDescription (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElements:@
setElements :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSArray value) => nsFetchIndexDescription -> value -> IO ()
setElements nsFetchIndexDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchIndexDescription (mkSelector "setElements:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- entity@
entity :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSEntityDescription)
entity nsFetchIndexDescription  =
  sendMsg nsFetchIndexDescription (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- partialIndexPredicate@
partialIndexPredicate :: IsNSFetchIndexDescription nsFetchIndexDescription => nsFetchIndexDescription -> IO (Id NSPredicate)
partialIndexPredicate nsFetchIndexDescription  =
  sendMsg nsFetchIndexDescription (mkSelector "partialIndexPredicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPartialIndexPredicate:@
setPartialIndexPredicate :: (IsNSFetchIndexDescription nsFetchIndexDescription, IsNSPredicate value) => nsFetchIndexDescription -> value -> IO ()
setPartialIndexPredicate nsFetchIndexDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchIndexDescription (mkSelector "setPartialIndexPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:elements:@
initWithName_elementsSelector :: Selector
initWithName_elementsSelector = mkSelector "initWithName:elements:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector
setElementsSelector = mkSelector "setElements:"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @partialIndexPredicate@
partialIndexPredicateSelector :: Selector
partialIndexPredicateSelector = mkSelector "partialIndexPredicate"

-- | @Selector@ for @setPartialIndexPredicate:@
setPartialIndexPredicateSelector :: Selector
setPartialIndexPredicateSelector = mkSelector "setPartialIndexPredicate:"

