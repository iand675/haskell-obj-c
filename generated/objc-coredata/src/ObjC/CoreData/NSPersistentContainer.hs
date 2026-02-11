{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentContainer@.
module ObjC.CoreData.NSPersistentContainer
  ( NSPersistentContainer
  , IsNSPersistentContainer(..)
  , persistentContainerWithName
  , persistentContainerWithName_managedObjectModel
  , defaultDirectoryURL
  , initWithName
  , initWithName_managedObjectModel
  , loadPersistentStoresWithCompletionHandler
  , newBackgroundContext
  , performBackgroundTask
  , name
  , viewContext
  , managedObjectModel
  , persistentStoreCoordinator
  , persistentStoreDescriptions
  , setPersistentStoreDescriptions
  , persistentContainerWithNameSelector
  , persistentContainerWithName_managedObjectModelSelector
  , defaultDirectoryURLSelector
  , initWithNameSelector
  , initWithName_managedObjectModelSelector
  , loadPersistentStoresWithCompletionHandlerSelector
  , newBackgroundContextSelector
  , performBackgroundTaskSelector
  , nameSelector
  , viewContextSelector
  , managedObjectModelSelector
  , persistentStoreCoordinatorSelector
  , persistentStoreDescriptionsSelector
  , setPersistentStoreDescriptionsSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ persistentContainerWithName:@
persistentContainerWithName :: IsNSString name => name -> IO (Id NSPersistentContainer)
persistentContainerWithName name =
  do
    cls' <- getRequiredClass "NSPersistentContainer"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "persistentContainerWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ persistentContainerWithName:managedObjectModel:@
persistentContainerWithName_managedObjectModel :: (IsNSString name, IsNSManagedObjectModel model) => name -> model -> IO (Id NSPersistentContainer)
persistentContainerWithName_managedObjectModel name model =
  do
    cls' <- getRequiredClass "NSPersistentContainer"
    withObjCPtr name $ \raw_name ->
      withObjCPtr model $ \raw_model ->
        sendClassMsg cls' (mkSelector "persistentContainerWithName:managedObjectModel:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_model :: Ptr ())] >>= retainedObject . castPtr

-- | @+ defaultDirectoryURL@
defaultDirectoryURL :: IO (Id NSURL)
defaultDirectoryURL  =
  do
    cls' <- getRequiredClass "NSPersistentContainer"
    sendClassMsg cls' (mkSelector "defaultDirectoryURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithName:@
initWithName :: (IsNSPersistentContainer nsPersistentContainer, IsNSString name) => nsPersistentContainer -> name -> IO (Id NSPersistentContainer)
initWithName nsPersistentContainer  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsPersistentContainer (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:managedObjectModel:@
initWithName_managedObjectModel :: (IsNSPersistentContainer nsPersistentContainer, IsNSString name, IsNSManagedObjectModel model) => nsPersistentContainer -> name -> model -> IO (Id NSPersistentContainer)
initWithName_managedObjectModel nsPersistentContainer  name model =
withObjCPtr name $ \raw_name ->
  withObjCPtr model $ \raw_model ->
      sendMsg nsPersistentContainer (mkSelector "initWithName:managedObjectModel:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_model :: Ptr ())] >>= ownedObject . castPtr

-- | @- loadPersistentStoresWithCompletionHandler:@
loadPersistentStoresWithCompletionHandler :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> Ptr () -> IO ()
loadPersistentStoresWithCompletionHandler nsPersistentContainer  block =
  sendMsg nsPersistentContainer (mkSelector "loadPersistentStoresWithCompletionHandler:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- newBackgroundContext@
newBackgroundContext :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSManagedObjectContext)
newBackgroundContext nsPersistentContainer  =
  sendMsg nsPersistentContainer (mkSelector "newBackgroundContext") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- performBackgroundTask:@
performBackgroundTask :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> Ptr () -> IO ()
performBackgroundTask nsPersistentContainer  block =
  sendMsg nsPersistentContainer (mkSelector "performBackgroundTask:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- name@
name :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSString)
name nsPersistentContainer  =
  sendMsg nsPersistentContainer (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- viewContext@
viewContext :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSManagedObjectContext)
viewContext nsPersistentContainer  =
  sendMsg nsPersistentContainer (mkSelector "viewContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- managedObjectModel@
managedObjectModel :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSManagedObjectModel)
managedObjectModel nsPersistentContainer  =
  sendMsg nsPersistentContainer (mkSelector "managedObjectModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- persistentStoreCoordinator@
persistentStoreCoordinator :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSPersistentStoreCoordinator)
persistentStoreCoordinator nsPersistentContainer  =
  sendMsg nsPersistentContainer (mkSelector "persistentStoreCoordinator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- persistentStoreDescriptions@
persistentStoreDescriptions :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSArray)
persistentStoreDescriptions nsPersistentContainer  =
  sendMsg nsPersistentContainer (mkSelector "persistentStoreDescriptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPersistentStoreDescriptions:@
setPersistentStoreDescriptions :: (IsNSPersistentContainer nsPersistentContainer, IsNSArray value) => nsPersistentContainer -> value -> IO ()
setPersistentStoreDescriptions nsPersistentContainer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentContainer (mkSelector "setPersistentStoreDescriptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistentContainerWithName:@
persistentContainerWithNameSelector :: Selector
persistentContainerWithNameSelector = mkSelector "persistentContainerWithName:"

-- | @Selector@ for @persistentContainerWithName:managedObjectModel:@
persistentContainerWithName_managedObjectModelSelector :: Selector
persistentContainerWithName_managedObjectModelSelector = mkSelector "persistentContainerWithName:managedObjectModel:"

-- | @Selector@ for @defaultDirectoryURL@
defaultDirectoryURLSelector :: Selector
defaultDirectoryURLSelector = mkSelector "defaultDirectoryURL"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:managedObjectModel:@
initWithName_managedObjectModelSelector :: Selector
initWithName_managedObjectModelSelector = mkSelector "initWithName:managedObjectModel:"

-- | @Selector@ for @loadPersistentStoresWithCompletionHandler:@
loadPersistentStoresWithCompletionHandlerSelector :: Selector
loadPersistentStoresWithCompletionHandlerSelector = mkSelector "loadPersistentStoresWithCompletionHandler:"

-- | @Selector@ for @newBackgroundContext@
newBackgroundContextSelector :: Selector
newBackgroundContextSelector = mkSelector "newBackgroundContext"

-- | @Selector@ for @performBackgroundTask:@
performBackgroundTaskSelector :: Selector
performBackgroundTaskSelector = mkSelector "performBackgroundTask:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @viewContext@
viewContextSelector :: Selector
viewContextSelector = mkSelector "viewContext"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector
managedObjectModelSelector = mkSelector "managedObjectModel"

-- | @Selector@ for @persistentStoreCoordinator@
persistentStoreCoordinatorSelector :: Selector
persistentStoreCoordinatorSelector = mkSelector "persistentStoreCoordinator"

-- | @Selector@ for @persistentStoreDescriptions@
persistentStoreDescriptionsSelector :: Selector
persistentStoreDescriptionsSelector = mkSelector "persistentStoreDescriptions"

-- | @Selector@ for @setPersistentStoreDescriptions:@
setPersistentStoreDescriptionsSelector :: Selector
setPersistentStoreDescriptionsSelector = mkSelector "setPersistentStoreDescriptions:"

