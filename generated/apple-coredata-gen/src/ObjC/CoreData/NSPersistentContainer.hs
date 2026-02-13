{-# LANGUAGE DataKinds #-}
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
  , defaultDirectoryURLSelector
  , initWithNameSelector
  , initWithName_managedObjectModelSelector
  , loadPersistentStoresWithCompletionHandlerSelector
  , managedObjectModelSelector
  , nameSelector
  , newBackgroundContextSelector
  , performBackgroundTaskSelector
  , persistentContainerWithNameSelector
  , persistentContainerWithName_managedObjectModelSelector
  , persistentStoreCoordinatorSelector
  , persistentStoreDescriptionsSelector
  , setPersistentStoreDescriptionsSelector
  , viewContextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ persistentContainerWithName:@
persistentContainerWithName :: IsNSString name => name -> IO (Id NSPersistentContainer)
persistentContainerWithName name =
  do
    cls' <- getRequiredClass "NSPersistentContainer"
    sendClassMessage cls' persistentContainerWithNameSelector (toNSString name)

-- | @+ persistentContainerWithName:managedObjectModel:@
persistentContainerWithName_managedObjectModel :: (IsNSString name, IsNSManagedObjectModel model) => name -> model -> IO (Id NSPersistentContainer)
persistentContainerWithName_managedObjectModel name model =
  do
    cls' <- getRequiredClass "NSPersistentContainer"
    sendClassMessage cls' persistentContainerWithName_managedObjectModelSelector (toNSString name) (toNSManagedObjectModel model)

-- | @+ defaultDirectoryURL@
defaultDirectoryURL :: IO (Id NSURL)
defaultDirectoryURL  =
  do
    cls' <- getRequiredClass "NSPersistentContainer"
    sendClassMessage cls' defaultDirectoryURLSelector

-- | @- initWithName:@
initWithName :: (IsNSPersistentContainer nsPersistentContainer, IsNSString name) => nsPersistentContainer -> name -> IO (Id NSPersistentContainer)
initWithName nsPersistentContainer name =
  sendOwnedMessage nsPersistentContainer initWithNameSelector (toNSString name)

-- | @- initWithName:managedObjectModel:@
initWithName_managedObjectModel :: (IsNSPersistentContainer nsPersistentContainer, IsNSString name, IsNSManagedObjectModel model) => nsPersistentContainer -> name -> model -> IO (Id NSPersistentContainer)
initWithName_managedObjectModel nsPersistentContainer name model =
  sendOwnedMessage nsPersistentContainer initWithName_managedObjectModelSelector (toNSString name) (toNSManagedObjectModel model)

-- | @- loadPersistentStoresWithCompletionHandler:@
loadPersistentStoresWithCompletionHandler :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> Ptr () -> IO ()
loadPersistentStoresWithCompletionHandler nsPersistentContainer block =
  sendMessage nsPersistentContainer loadPersistentStoresWithCompletionHandlerSelector block

-- | @- newBackgroundContext@
newBackgroundContext :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSManagedObjectContext)
newBackgroundContext nsPersistentContainer =
  sendOwnedMessage nsPersistentContainer newBackgroundContextSelector

-- | @- performBackgroundTask:@
performBackgroundTask :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> Ptr () -> IO ()
performBackgroundTask nsPersistentContainer block =
  sendMessage nsPersistentContainer performBackgroundTaskSelector block

-- | @- name@
name :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSString)
name nsPersistentContainer =
  sendMessage nsPersistentContainer nameSelector

-- | @- viewContext@
viewContext :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSManagedObjectContext)
viewContext nsPersistentContainer =
  sendMessage nsPersistentContainer viewContextSelector

-- | @- managedObjectModel@
managedObjectModel :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSManagedObjectModel)
managedObjectModel nsPersistentContainer =
  sendMessage nsPersistentContainer managedObjectModelSelector

-- | @- persistentStoreCoordinator@
persistentStoreCoordinator :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSPersistentStoreCoordinator)
persistentStoreCoordinator nsPersistentContainer =
  sendMessage nsPersistentContainer persistentStoreCoordinatorSelector

-- | @- persistentStoreDescriptions@
persistentStoreDescriptions :: IsNSPersistentContainer nsPersistentContainer => nsPersistentContainer -> IO (Id NSArray)
persistentStoreDescriptions nsPersistentContainer =
  sendMessage nsPersistentContainer persistentStoreDescriptionsSelector

-- | @- setPersistentStoreDescriptions:@
setPersistentStoreDescriptions :: (IsNSPersistentContainer nsPersistentContainer, IsNSArray value) => nsPersistentContainer -> value -> IO ()
setPersistentStoreDescriptions nsPersistentContainer value =
  sendMessage nsPersistentContainer setPersistentStoreDescriptionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistentContainerWithName:@
persistentContainerWithNameSelector :: Selector '[Id NSString] (Id NSPersistentContainer)
persistentContainerWithNameSelector = mkSelector "persistentContainerWithName:"

-- | @Selector@ for @persistentContainerWithName:managedObjectModel:@
persistentContainerWithName_managedObjectModelSelector :: Selector '[Id NSString, Id NSManagedObjectModel] (Id NSPersistentContainer)
persistentContainerWithName_managedObjectModelSelector = mkSelector "persistentContainerWithName:managedObjectModel:"

-- | @Selector@ for @defaultDirectoryURL@
defaultDirectoryURLSelector :: Selector '[] (Id NSURL)
defaultDirectoryURLSelector = mkSelector "defaultDirectoryURL"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id NSPersistentContainer)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:managedObjectModel:@
initWithName_managedObjectModelSelector :: Selector '[Id NSString, Id NSManagedObjectModel] (Id NSPersistentContainer)
initWithName_managedObjectModelSelector = mkSelector "initWithName:managedObjectModel:"

-- | @Selector@ for @loadPersistentStoresWithCompletionHandler:@
loadPersistentStoresWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadPersistentStoresWithCompletionHandlerSelector = mkSelector "loadPersistentStoresWithCompletionHandler:"

-- | @Selector@ for @newBackgroundContext@
newBackgroundContextSelector :: Selector '[] (Id NSManagedObjectContext)
newBackgroundContextSelector = mkSelector "newBackgroundContext"

-- | @Selector@ for @performBackgroundTask:@
performBackgroundTaskSelector :: Selector '[Ptr ()] ()
performBackgroundTaskSelector = mkSelector "performBackgroundTask:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @viewContext@
viewContextSelector :: Selector '[] (Id NSManagedObjectContext)
viewContextSelector = mkSelector "viewContext"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector '[] (Id NSManagedObjectModel)
managedObjectModelSelector = mkSelector "managedObjectModel"

-- | @Selector@ for @persistentStoreCoordinator@
persistentStoreCoordinatorSelector :: Selector '[] (Id NSPersistentStoreCoordinator)
persistentStoreCoordinatorSelector = mkSelector "persistentStoreCoordinator"

-- | @Selector@ for @persistentStoreDescriptions@
persistentStoreDescriptionsSelector :: Selector '[] (Id NSArray)
persistentStoreDescriptionsSelector = mkSelector "persistentStoreDescriptions"

-- | @Selector@ for @setPersistentStoreDescriptions:@
setPersistentStoreDescriptionsSelector :: Selector '[Id NSArray] ()
setPersistentStoreDescriptionsSelector = mkSelector "setPersistentStoreDescriptions:"

