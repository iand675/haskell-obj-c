{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFetchedResultsController@.
module ObjC.CoreData.NSFetchedResultsController
  ( NSFetchedResultsController
  , IsNSFetchedResultsController(..)
  , initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheName
  , performFetch
  , deleteCacheWithName
  , sectionIndexTitleForSectionName
  , sectionForSectionIndexTitle_atIndex
  , fetchRequest
  , managedObjectContext
  , sectionNameKeyPath
  , cacheName
  , delegate
  , setDelegate
  , fetchedObjects
  , sectionIndexTitles
  , sections
  , cacheNameSelector
  , delegateSelector
  , deleteCacheWithNameSelector
  , fetchRequestSelector
  , fetchedObjectsSelector
  , initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector
  , managedObjectContextSelector
  , performFetchSelector
  , sectionForSectionIndexTitle_atIndexSelector
  , sectionIndexTitleForSectionNameSelector
  , sectionIndexTitlesSelector
  , sectionNameKeyPathSelector
  , sectionsSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:@
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheName :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSFetchRequest fetchRequest, IsNSManagedObjectContext context, IsNSString sectionNameKeyPath, IsNSString name) => nsFetchedResultsController -> fetchRequest -> context -> sectionNameKeyPath -> name -> IO (Id NSFetchedResultsController)
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheName nsFetchedResultsController fetchRequest context sectionNameKeyPath name =
  sendOwnedMessage nsFetchedResultsController initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector (toNSFetchRequest fetchRequest) (toNSManagedObjectContext context) (toNSString sectionNameKeyPath) (toNSString name)

-- | @- performFetch:@
performFetch :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSError error_) => nsFetchedResultsController -> error_ -> IO Bool
performFetch nsFetchedResultsController error_ =
  sendMessage nsFetchedResultsController performFetchSelector (toNSError error_)

-- | @+ deleteCacheWithName:@
deleteCacheWithName :: IsNSString name => name -> IO ()
deleteCacheWithName name =
  do
    cls' <- getRequiredClass "NSFetchedResultsController"
    sendClassMessage cls' deleteCacheWithNameSelector (toNSString name)

-- | @- sectionIndexTitleForSectionName:@
sectionIndexTitleForSectionName :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSString sectionName) => nsFetchedResultsController -> sectionName -> IO (Id NSString)
sectionIndexTitleForSectionName nsFetchedResultsController sectionName =
  sendMessage nsFetchedResultsController sectionIndexTitleForSectionNameSelector (toNSString sectionName)

-- | @- sectionForSectionIndexTitle:atIndex:@
sectionForSectionIndexTitle_atIndex :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSString title) => nsFetchedResultsController -> title -> CLong -> IO CLong
sectionForSectionIndexTitle_atIndex nsFetchedResultsController title sectionIndex =
  sendMessage nsFetchedResultsController sectionForSectionIndexTitle_atIndexSelector (toNSString title) sectionIndex

-- | @- fetchRequest@
fetchRequest :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSFetchRequest)
fetchRequest nsFetchedResultsController =
  sendMessage nsFetchedResultsController fetchRequestSelector

-- | @- managedObjectContext@
managedObjectContext :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSManagedObjectContext)
managedObjectContext nsFetchedResultsController =
  sendMessage nsFetchedResultsController managedObjectContextSelector

-- | @- sectionNameKeyPath@
sectionNameKeyPath :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSString)
sectionNameKeyPath nsFetchedResultsController =
  sendMessage nsFetchedResultsController sectionNameKeyPathSelector

-- | @- cacheName@
cacheName :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSString)
cacheName nsFetchedResultsController =
  sendMessage nsFetchedResultsController cacheNameSelector

-- | @- delegate@
delegate :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO RawId
delegate nsFetchedResultsController =
  sendMessage nsFetchedResultsController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> RawId -> IO ()
setDelegate nsFetchedResultsController value =
  sendMessage nsFetchedResultsController setDelegateSelector value

-- | @- fetchedObjects@
fetchedObjects :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSArray)
fetchedObjects nsFetchedResultsController =
  sendMessage nsFetchedResultsController fetchedObjectsSelector

-- | @- sectionIndexTitles@
sectionIndexTitles :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSArray)
sectionIndexTitles nsFetchedResultsController =
  sendMessage nsFetchedResultsController sectionIndexTitlesSelector

-- | @- sections@
sections :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSArray)
sections nsFetchedResultsController =
  sendMessage nsFetchedResultsController sectionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:@
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector :: Selector '[Id NSFetchRequest, Id NSManagedObjectContext, Id NSString, Id NSString] (Id NSFetchedResultsController)
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector = mkSelector "initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:"

-- | @Selector@ for @performFetch:@
performFetchSelector :: Selector '[Id NSError] Bool
performFetchSelector = mkSelector "performFetch:"

-- | @Selector@ for @deleteCacheWithName:@
deleteCacheWithNameSelector :: Selector '[Id NSString] ()
deleteCacheWithNameSelector = mkSelector "deleteCacheWithName:"

-- | @Selector@ for @sectionIndexTitleForSectionName:@
sectionIndexTitleForSectionNameSelector :: Selector '[Id NSString] (Id NSString)
sectionIndexTitleForSectionNameSelector = mkSelector "sectionIndexTitleForSectionName:"

-- | @Selector@ for @sectionForSectionIndexTitle:atIndex:@
sectionForSectionIndexTitle_atIndexSelector :: Selector '[Id NSString, CLong] CLong
sectionForSectionIndexTitle_atIndexSelector = mkSelector "sectionForSectionIndexTitle:atIndex:"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector '[] (Id NSManagedObjectContext)
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @sectionNameKeyPath@
sectionNameKeyPathSelector :: Selector '[] (Id NSString)
sectionNameKeyPathSelector = mkSelector "sectionNameKeyPath"

-- | @Selector@ for @cacheName@
cacheNameSelector :: Selector '[] (Id NSString)
cacheNameSelector = mkSelector "cacheName"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @fetchedObjects@
fetchedObjectsSelector :: Selector '[] (Id NSArray)
fetchedObjectsSelector = mkSelector "fetchedObjects"

-- | @Selector@ for @sectionIndexTitles@
sectionIndexTitlesSelector :: Selector '[] (Id NSArray)
sectionIndexTitlesSelector = mkSelector "sectionIndexTitles"

-- | @Selector@ for @sections@
sectionsSelector :: Selector '[] (Id NSArray)
sectionsSelector = mkSelector "sections"

