{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObjectController@.
module ObjC.AppKit.NSObjectController
  ( NSObjectController
  , IsNSObjectController(..)
  , initWithContent
  , initWithCoder
  , prepareContent
  , newObject
  , addObject
  , removeObject
  , add
  , remove
  , validateUserInterfaceItem
  , fetchWithRequest_merge_error
  , fetch
  , defaultFetchRequest
  , content
  , setContent
  , selection
  , selectedObjects
  , automaticallyPreparesContent
  , setAutomaticallyPreparesContent
  , objectClass
  , setObjectClass
  , editable
  , setEditable
  , canAdd
  , canRemove
  , managedObjectContext
  , setManagedObjectContext
  , entityName
  , setEntityName
  , fetchPredicate
  , setFetchPredicate
  , usesLazyFetching
  , setUsesLazyFetching
  , addObjectSelector
  , addSelector
  , automaticallyPreparesContentSelector
  , canAddSelector
  , canRemoveSelector
  , contentSelector
  , defaultFetchRequestSelector
  , editableSelector
  , entityNameSelector
  , fetchPredicateSelector
  , fetchSelector
  , fetchWithRequest_merge_errorSelector
  , initWithCoderSelector
  , initWithContentSelector
  , managedObjectContextSelector
  , newObjectSelector
  , objectClassSelector
  , prepareContentSelector
  , removeObjectSelector
  , removeSelector
  , selectedObjectsSelector
  , selectionSelector
  , setAutomaticallyPreparesContentSelector
  , setContentSelector
  , setEditableSelector
  , setEntityNameSelector
  , setFetchPredicateSelector
  , setManagedObjectContextSelector
  , setObjectClassSelector
  , setUsesLazyFetchingSelector
  , usesLazyFetchingSelector
  , validateUserInterfaceItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithContent:@
initWithContent :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO (Id NSObjectController)
initWithContent nsObjectController content =
  sendOwnedMessage nsObjectController initWithContentSelector content

-- | @- initWithCoder:@
initWithCoder :: (IsNSObjectController nsObjectController, IsNSCoder coder) => nsObjectController -> coder -> IO (Id NSObjectController)
initWithCoder nsObjectController coder =
  sendOwnedMessage nsObjectController initWithCoderSelector (toNSCoder coder)

-- | @- prepareContent@
prepareContent :: IsNSObjectController nsObjectController => nsObjectController -> IO ()
prepareContent nsObjectController =
  sendMessage nsObjectController prepareContentSelector

-- | @- newObject@
newObject :: IsNSObjectController nsObjectController => nsObjectController -> IO RawId
newObject nsObjectController =
  sendOwnedMessage nsObjectController newObjectSelector

-- | @- addObject:@
addObject :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
addObject nsObjectController object =
  sendMessage nsObjectController addObjectSelector object

-- | @- removeObject:@
removeObject :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
removeObject nsObjectController object =
  sendMessage nsObjectController removeObjectSelector object

-- | @- add:@
add :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
add nsObjectController sender =
  sendMessage nsObjectController addSelector sender

-- | @- remove:@
remove :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
remove nsObjectController sender =
  sendMessage nsObjectController removeSelector sender

-- | @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO Bool
validateUserInterfaceItem nsObjectController item =
  sendMessage nsObjectController validateUserInterfaceItemSelector item

-- | @- fetchWithRequest:merge:error:@
fetchWithRequest_merge_error :: (IsNSObjectController nsObjectController, IsNSFetchRequest fetchRequest, IsNSError error_) => nsObjectController -> fetchRequest -> Bool -> error_ -> IO Bool
fetchWithRequest_merge_error nsObjectController fetchRequest merge error_ =
  sendMessage nsObjectController fetchWithRequest_merge_errorSelector (toNSFetchRequest fetchRequest) merge (toNSError error_)

-- | @- fetch:@
fetch :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
fetch nsObjectController sender =
  sendMessage nsObjectController fetchSelector sender

-- | @- defaultFetchRequest@
defaultFetchRequest :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSFetchRequest)
defaultFetchRequest nsObjectController =
  sendMessage nsObjectController defaultFetchRequestSelector

-- | @- content@
content :: IsNSObjectController nsObjectController => nsObjectController -> IO RawId
content nsObjectController =
  sendMessage nsObjectController contentSelector

-- | @- setContent:@
setContent :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
setContent nsObjectController value =
  sendMessage nsObjectController setContentSelector value

-- | @- selection@
selection :: IsNSObjectController nsObjectController => nsObjectController -> IO RawId
selection nsObjectController =
  sendMessage nsObjectController selectionSelector

-- | @- selectedObjects@
selectedObjects :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSArray)
selectedObjects nsObjectController =
  sendMessage nsObjectController selectedObjectsSelector

-- | @- automaticallyPreparesContent@
automaticallyPreparesContent :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
automaticallyPreparesContent nsObjectController =
  sendMessage nsObjectController automaticallyPreparesContentSelector

-- | @- setAutomaticallyPreparesContent:@
setAutomaticallyPreparesContent :: IsNSObjectController nsObjectController => nsObjectController -> Bool -> IO ()
setAutomaticallyPreparesContent nsObjectController value =
  sendMessage nsObjectController setAutomaticallyPreparesContentSelector value

-- | @- objectClass@
objectClass :: IsNSObjectController nsObjectController => nsObjectController -> IO Class
objectClass nsObjectController =
  sendMessage nsObjectController objectClassSelector

-- | @- setObjectClass:@
setObjectClass :: IsNSObjectController nsObjectController => nsObjectController -> Class -> IO ()
setObjectClass nsObjectController value =
  sendMessage nsObjectController setObjectClassSelector value

-- | @- editable@
editable :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
editable nsObjectController =
  sendMessage nsObjectController editableSelector

-- | @- setEditable:@
setEditable :: IsNSObjectController nsObjectController => nsObjectController -> Bool -> IO ()
setEditable nsObjectController value =
  sendMessage nsObjectController setEditableSelector value

-- | @- canAdd@
canAdd :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
canAdd nsObjectController =
  sendMessage nsObjectController canAddSelector

-- | @- canRemove@
canRemove :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
canRemove nsObjectController =
  sendMessage nsObjectController canRemoveSelector

-- | @- managedObjectContext@
managedObjectContext :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSManagedObjectContext)
managedObjectContext nsObjectController =
  sendMessage nsObjectController managedObjectContextSelector

-- | @- setManagedObjectContext:@
setManagedObjectContext :: (IsNSObjectController nsObjectController, IsNSManagedObjectContext value) => nsObjectController -> value -> IO ()
setManagedObjectContext nsObjectController value =
  sendMessage nsObjectController setManagedObjectContextSelector (toNSManagedObjectContext value)

-- | @- entityName@
entityName :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSString)
entityName nsObjectController =
  sendMessage nsObjectController entityNameSelector

-- | @- setEntityName:@
setEntityName :: (IsNSObjectController nsObjectController, IsNSString value) => nsObjectController -> value -> IO ()
setEntityName nsObjectController value =
  sendMessage nsObjectController setEntityNameSelector (toNSString value)

-- | @- fetchPredicate@
fetchPredicate :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSPredicate)
fetchPredicate nsObjectController =
  sendMessage nsObjectController fetchPredicateSelector

-- | @- setFetchPredicate:@
setFetchPredicate :: (IsNSObjectController nsObjectController, IsNSPredicate value) => nsObjectController -> value -> IO ()
setFetchPredicate nsObjectController value =
  sendMessage nsObjectController setFetchPredicateSelector (toNSPredicate value)

-- | @- usesLazyFetching@
usesLazyFetching :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
usesLazyFetching nsObjectController =
  sendMessage nsObjectController usesLazyFetchingSelector

-- | @- setUsesLazyFetching:@
setUsesLazyFetching :: IsNSObjectController nsObjectController => nsObjectController -> Bool -> IO ()
setUsesLazyFetching nsObjectController value =
  sendMessage nsObjectController setUsesLazyFetchingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector '[RawId] (Id NSObjectController)
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSObjectController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @prepareContent@
prepareContentSelector :: Selector '[] ()
prepareContentSelector = mkSelector "prepareContent"

-- | @Selector@ for @newObject@
newObjectSelector :: Selector '[] RawId
newObjectSelector = mkSelector "newObject"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector '[RawId] ()
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @add:@
addSelector :: Selector '[RawId] ()
addSelector = mkSelector "add:"

-- | @Selector@ for @remove:@
removeSelector :: Selector '[RawId] ()
removeSelector = mkSelector "remove:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector '[RawId] Bool
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @fetchWithRequest:merge:error:@
fetchWithRequest_merge_errorSelector :: Selector '[Id NSFetchRequest, Bool, Id NSError] Bool
fetchWithRequest_merge_errorSelector = mkSelector "fetchWithRequest:merge:error:"

-- | @Selector@ for @fetch:@
fetchSelector :: Selector '[RawId] ()
fetchSelector = mkSelector "fetch:"

-- | @Selector@ for @defaultFetchRequest@
defaultFetchRequestSelector :: Selector '[] (Id NSFetchRequest)
defaultFetchRequestSelector = mkSelector "defaultFetchRequest"

-- | @Selector@ for @content@
contentSelector :: Selector '[] RawId
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector '[RawId] ()
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @selection@
selectionSelector :: Selector '[] RawId
selectionSelector = mkSelector "selection"

-- | @Selector@ for @selectedObjects@
selectedObjectsSelector :: Selector '[] (Id NSArray)
selectedObjectsSelector = mkSelector "selectedObjects"

-- | @Selector@ for @automaticallyPreparesContent@
automaticallyPreparesContentSelector :: Selector '[] Bool
automaticallyPreparesContentSelector = mkSelector "automaticallyPreparesContent"

-- | @Selector@ for @setAutomaticallyPreparesContent:@
setAutomaticallyPreparesContentSelector :: Selector '[Bool] ()
setAutomaticallyPreparesContentSelector = mkSelector "setAutomaticallyPreparesContent:"

-- | @Selector@ for @objectClass@
objectClassSelector :: Selector '[] Class
objectClassSelector = mkSelector "objectClass"

-- | @Selector@ for @setObjectClass:@
setObjectClassSelector :: Selector '[Class] ()
setObjectClassSelector = mkSelector "setObjectClass:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @canAdd@
canAddSelector :: Selector '[] Bool
canAddSelector = mkSelector "canAdd"

-- | @Selector@ for @canRemove@
canRemoveSelector :: Selector '[] Bool
canRemoveSelector = mkSelector "canRemove"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector '[] (Id NSManagedObjectContext)
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @setManagedObjectContext:@
setManagedObjectContextSelector :: Selector '[Id NSManagedObjectContext] ()
setManagedObjectContextSelector = mkSelector "setManagedObjectContext:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector '[] (Id NSString)
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @setEntityName:@
setEntityNameSelector :: Selector '[Id NSString] ()
setEntityNameSelector = mkSelector "setEntityName:"

-- | @Selector@ for @fetchPredicate@
fetchPredicateSelector :: Selector '[] (Id NSPredicate)
fetchPredicateSelector = mkSelector "fetchPredicate"

-- | @Selector@ for @setFetchPredicate:@
setFetchPredicateSelector :: Selector '[Id NSPredicate] ()
setFetchPredicateSelector = mkSelector "setFetchPredicate:"

-- | @Selector@ for @usesLazyFetching@
usesLazyFetchingSelector :: Selector '[] Bool
usesLazyFetchingSelector = mkSelector "usesLazyFetching"

-- | @Selector@ for @setUsesLazyFetching:@
setUsesLazyFetchingSelector :: Selector '[Bool] ()
setUsesLazyFetchingSelector = mkSelector "setUsesLazyFetching:"

