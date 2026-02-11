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
  , initWithContentSelector
  , initWithCoderSelector
  , prepareContentSelector
  , newObjectSelector
  , addObjectSelector
  , removeObjectSelector
  , addSelector
  , removeSelector
  , validateUserInterfaceItemSelector
  , fetchWithRequest_merge_errorSelector
  , fetchSelector
  , defaultFetchRequestSelector
  , contentSelector
  , setContentSelector
  , selectionSelector
  , selectedObjectsSelector
  , automaticallyPreparesContentSelector
  , setAutomaticallyPreparesContentSelector
  , objectClassSelector
  , setObjectClassSelector
  , editableSelector
  , setEditableSelector
  , canAddSelector
  , canRemoveSelector
  , managedObjectContextSelector
  , setManagedObjectContextSelector
  , entityNameSelector
  , setEntityNameSelector
  , fetchPredicateSelector
  , setFetchPredicateSelector
  , usesLazyFetchingSelector
  , setUsesLazyFetchingSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithContent:@
initWithContent :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO (Id NSObjectController)
initWithContent nsObjectController  content =
  sendMsg nsObjectController (mkSelector "initWithContent:") (retPtr retVoid) [argPtr (castPtr (unRawId content) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSObjectController nsObjectController, IsNSCoder coder) => nsObjectController -> coder -> IO (Id NSObjectController)
initWithCoder nsObjectController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsObjectController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- prepareContent@
prepareContent :: IsNSObjectController nsObjectController => nsObjectController -> IO ()
prepareContent nsObjectController  =
  sendMsg nsObjectController (mkSelector "prepareContent") retVoid []

-- | @- newObject@
newObject :: IsNSObjectController nsObjectController => nsObjectController -> IO RawId
newObject nsObjectController  =
  fmap (RawId . castPtr) $ sendMsg nsObjectController (mkSelector "newObject") (retPtr retVoid) []

-- | @- addObject:@
addObject :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
addObject nsObjectController  object =
  sendMsg nsObjectController (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeObject:@
removeObject :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
removeObject nsObjectController  object =
  sendMsg nsObjectController (mkSelector "removeObject:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- add:@
add :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
add nsObjectController  sender =
  sendMsg nsObjectController (mkSelector "add:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- remove:@
remove :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
remove nsObjectController  sender =
  sendMsg nsObjectController (mkSelector "remove:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO Bool
validateUserInterfaceItem nsObjectController  item =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "validateUserInterfaceItem:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- fetchWithRequest:merge:error:@
fetchWithRequest_merge_error :: (IsNSObjectController nsObjectController, IsNSFetchRequest fetchRequest, IsNSError error_) => nsObjectController -> fetchRequest -> Bool -> error_ -> IO Bool
fetchWithRequest_merge_error nsObjectController  fetchRequest merge error_ =
withObjCPtr fetchRequest $ \raw_fetchRequest ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "fetchWithRequest:merge:error:") retCULong [argPtr (castPtr raw_fetchRequest :: Ptr ()), argCULong (if merge then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- fetch:@
fetch :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
fetch nsObjectController  sender =
  sendMsg nsObjectController (mkSelector "fetch:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- defaultFetchRequest@
defaultFetchRequest :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSFetchRequest)
defaultFetchRequest nsObjectController  =
  sendMsg nsObjectController (mkSelector "defaultFetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- content@
content :: IsNSObjectController nsObjectController => nsObjectController -> IO RawId
content nsObjectController  =
  fmap (RawId . castPtr) $ sendMsg nsObjectController (mkSelector "content") (retPtr retVoid) []

-- | @- setContent:@
setContent :: IsNSObjectController nsObjectController => nsObjectController -> RawId -> IO ()
setContent nsObjectController  value =
  sendMsg nsObjectController (mkSelector "setContent:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- selection@
selection :: IsNSObjectController nsObjectController => nsObjectController -> IO RawId
selection nsObjectController  =
  fmap (RawId . castPtr) $ sendMsg nsObjectController (mkSelector "selection") (retPtr retVoid) []

-- | @- selectedObjects@
selectedObjects :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSArray)
selectedObjects nsObjectController  =
  sendMsg nsObjectController (mkSelector "selectedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- automaticallyPreparesContent@
automaticallyPreparesContent :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
automaticallyPreparesContent nsObjectController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "automaticallyPreparesContent") retCULong []

-- | @- setAutomaticallyPreparesContent:@
setAutomaticallyPreparesContent :: IsNSObjectController nsObjectController => nsObjectController -> Bool -> IO ()
setAutomaticallyPreparesContent nsObjectController  value =
  sendMsg nsObjectController (mkSelector "setAutomaticallyPreparesContent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- objectClass@
objectClass :: IsNSObjectController nsObjectController => nsObjectController -> IO Class
objectClass nsObjectController  =
  fmap (Class . castPtr) $ sendMsg nsObjectController (mkSelector "objectClass") (retPtr retVoid) []

-- | @- setObjectClass:@
setObjectClass :: IsNSObjectController nsObjectController => nsObjectController -> Class -> IO ()
setObjectClass nsObjectController  value =
  sendMsg nsObjectController (mkSelector "setObjectClass:") retVoid [argPtr (unClass value)]

-- | @- editable@
editable :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
editable nsObjectController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSObjectController nsObjectController => nsObjectController -> Bool -> IO ()
setEditable nsObjectController  value =
  sendMsg nsObjectController (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canAdd@
canAdd :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
canAdd nsObjectController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "canAdd") retCULong []

-- | @- canRemove@
canRemove :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
canRemove nsObjectController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "canRemove") retCULong []

-- | @- managedObjectContext@
managedObjectContext :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSManagedObjectContext)
managedObjectContext nsObjectController  =
  sendMsg nsObjectController (mkSelector "managedObjectContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManagedObjectContext:@
setManagedObjectContext :: (IsNSObjectController nsObjectController, IsNSManagedObjectContext value) => nsObjectController -> value -> IO ()
setManagedObjectContext nsObjectController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsObjectController (mkSelector "setManagedObjectContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- entityName@
entityName :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSString)
entityName nsObjectController  =
  sendMsg nsObjectController (mkSelector "entityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEntityName:@
setEntityName :: (IsNSObjectController nsObjectController, IsNSString value) => nsObjectController -> value -> IO ()
setEntityName nsObjectController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsObjectController (mkSelector "setEntityName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fetchPredicate@
fetchPredicate :: IsNSObjectController nsObjectController => nsObjectController -> IO (Id NSPredicate)
fetchPredicate nsObjectController  =
  sendMsg nsObjectController (mkSelector "fetchPredicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFetchPredicate:@
setFetchPredicate :: (IsNSObjectController nsObjectController, IsNSPredicate value) => nsObjectController -> value -> IO ()
setFetchPredicate nsObjectController  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsObjectController (mkSelector "setFetchPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- usesLazyFetching@
usesLazyFetching :: IsNSObjectController nsObjectController => nsObjectController -> IO Bool
usesLazyFetching nsObjectController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObjectController (mkSelector "usesLazyFetching") retCULong []

-- | @- setUsesLazyFetching:@
setUsesLazyFetching :: IsNSObjectController nsObjectController => nsObjectController -> Bool -> IO ()
setUsesLazyFetching nsObjectController  value =
  sendMsg nsObjectController (mkSelector "setUsesLazyFetching:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @prepareContent@
prepareContentSelector :: Selector
prepareContentSelector = mkSelector "prepareContent"

-- | @Selector@ for @newObject@
newObjectSelector :: Selector
newObjectSelector = mkSelector "newObject"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @removeObject:@
removeObjectSelector :: Selector
removeObjectSelector = mkSelector "removeObject:"

-- | @Selector@ for @add:@
addSelector :: Selector
addSelector = mkSelector "add:"

-- | @Selector@ for @remove:@
removeSelector :: Selector
removeSelector = mkSelector "remove:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @fetchWithRequest:merge:error:@
fetchWithRequest_merge_errorSelector :: Selector
fetchWithRequest_merge_errorSelector = mkSelector "fetchWithRequest:merge:error:"

-- | @Selector@ for @fetch:@
fetchSelector :: Selector
fetchSelector = mkSelector "fetch:"

-- | @Selector@ for @defaultFetchRequest@
defaultFetchRequestSelector :: Selector
defaultFetchRequestSelector = mkSelector "defaultFetchRequest"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @setContent:@
setContentSelector :: Selector
setContentSelector = mkSelector "setContent:"

-- | @Selector@ for @selection@
selectionSelector :: Selector
selectionSelector = mkSelector "selection"

-- | @Selector@ for @selectedObjects@
selectedObjectsSelector :: Selector
selectedObjectsSelector = mkSelector "selectedObjects"

-- | @Selector@ for @automaticallyPreparesContent@
automaticallyPreparesContentSelector :: Selector
automaticallyPreparesContentSelector = mkSelector "automaticallyPreparesContent"

-- | @Selector@ for @setAutomaticallyPreparesContent:@
setAutomaticallyPreparesContentSelector :: Selector
setAutomaticallyPreparesContentSelector = mkSelector "setAutomaticallyPreparesContent:"

-- | @Selector@ for @objectClass@
objectClassSelector :: Selector
objectClassSelector = mkSelector "objectClass"

-- | @Selector@ for @setObjectClass:@
setObjectClassSelector :: Selector
setObjectClassSelector = mkSelector "setObjectClass:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @canAdd@
canAddSelector :: Selector
canAddSelector = mkSelector "canAdd"

-- | @Selector@ for @canRemove@
canRemoveSelector :: Selector
canRemoveSelector = mkSelector "canRemove"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @setManagedObjectContext:@
setManagedObjectContextSelector :: Selector
setManagedObjectContextSelector = mkSelector "setManagedObjectContext:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @setEntityName:@
setEntityNameSelector :: Selector
setEntityNameSelector = mkSelector "setEntityName:"

-- | @Selector@ for @fetchPredicate@
fetchPredicateSelector :: Selector
fetchPredicateSelector = mkSelector "fetchPredicate"

-- | @Selector@ for @setFetchPredicate:@
setFetchPredicateSelector :: Selector
setFetchPredicateSelector = mkSelector "setFetchPredicate:"

-- | @Selector@ for @usesLazyFetching@
usesLazyFetchingSelector :: Selector
usesLazyFetchingSelector = mkSelector "usesLazyFetching"

-- | @Selector@ for @setUsesLazyFetching:@
setUsesLazyFetchingSelector :: Selector
setUsesLazyFetchingSelector = mkSelector "setUsesLazyFetching:"

