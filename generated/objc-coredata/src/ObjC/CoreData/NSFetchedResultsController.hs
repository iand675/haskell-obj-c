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
  , fetchedObjects
  , sectionIndexTitles
  , initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector
  , performFetchSelector
  , deleteCacheWithNameSelector
  , sectionIndexTitleForSectionNameSelector
  , sectionForSectionIndexTitle_atIndexSelector
  , fetchRequestSelector
  , managedObjectContextSelector
  , sectionNameKeyPathSelector
  , cacheNameSelector
  , fetchedObjectsSelector
  , sectionIndexTitlesSelector


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

-- | @- initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:@
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheName :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSFetchRequest fetchRequest, IsNSManagedObjectContext context, IsNSString sectionNameKeyPath, IsNSString name) => nsFetchedResultsController -> fetchRequest -> context -> sectionNameKeyPath -> name -> IO (Id NSFetchedResultsController)
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheName nsFetchedResultsController  fetchRequest context sectionNameKeyPath name =
withObjCPtr fetchRequest $ \raw_fetchRequest ->
  withObjCPtr context $ \raw_context ->
    withObjCPtr sectionNameKeyPath $ \raw_sectionNameKeyPath ->
      withObjCPtr name $ \raw_name ->
          sendMsg nsFetchedResultsController (mkSelector "initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:") (retPtr retVoid) [argPtr (castPtr raw_fetchRequest :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_sectionNameKeyPath :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- performFetch:@
performFetch :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSError error_) => nsFetchedResultsController -> error_ -> IO Bool
performFetch nsFetchedResultsController  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchedResultsController (mkSelector "performFetch:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ deleteCacheWithName:@
deleteCacheWithName :: IsNSString name => name -> IO ()
deleteCacheWithName name =
  do
    cls' <- getRequiredClass "NSFetchedResultsController"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "deleteCacheWithName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- sectionIndexTitleForSectionName:@
sectionIndexTitleForSectionName :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSString sectionName) => nsFetchedResultsController -> sectionName -> IO (Id NSString)
sectionIndexTitleForSectionName nsFetchedResultsController  sectionName =
withObjCPtr sectionName $ \raw_sectionName ->
    sendMsg nsFetchedResultsController (mkSelector "sectionIndexTitleForSectionName:") (retPtr retVoid) [argPtr (castPtr raw_sectionName :: Ptr ())] >>= retainedObject . castPtr

-- | @- sectionForSectionIndexTitle:atIndex:@
sectionForSectionIndexTitle_atIndex :: (IsNSFetchedResultsController nsFetchedResultsController, IsNSString title) => nsFetchedResultsController -> title -> CLong -> IO CLong
sectionForSectionIndexTitle_atIndex nsFetchedResultsController  title sectionIndex =
withObjCPtr title $ \raw_title ->
    sendMsg nsFetchedResultsController (mkSelector "sectionForSectionIndexTitle:atIndex:") retCLong [argPtr (castPtr raw_title :: Ptr ()), argCLong (fromIntegral sectionIndex)]

-- | @- fetchRequest@
fetchRequest :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSFetchRequest)
fetchRequest nsFetchedResultsController  =
  sendMsg nsFetchedResultsController (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- managedObjectContext@
managedObjectContext :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSManagedObjectContext)
managedObjectContext nsFetchedResultsController  =
  sendMsg nsFetchedResultsController (mkSelector "managedObjectContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sectionNameKeyPath@
sectionNameKeyPath :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSString)
sectionNameKeyPath nsFetchedResultsController  =
  sendMsg nsFetchedResultsController (mkSelector "sectionNameKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cacheName@
cacheName :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSString)
cacheName nsFetchedResultsController  =
  sendMsg nsFetchedResultsController (mkSelector "cacheName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchedObjects@
fetchedObjects :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSArray)
fetchedObjects nsFetchedResultsController  =
  sendMsg nsFetchedResultsController (mkSelector "fetchedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sectionIndexTitles@
sectionIndexTitles :: IsNSFetchedResultsController nsFetchedResultsController => nsFetchedResultsController -> IO (Id NSArray)
sectionIndexTitles nsFetchedResultsController  =
  sendMsg nsFetchedResultsController (mkSelector "sectionIndexTitles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:@
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector :: Selector
initWithFetchRequest_managedObjectContext_sectionNameKeyPath_cacheNameSelector = mkSelector "initWithFetchRequest:managedObjectContext:sectionNameKeyPath:cacheName:"

-- | @Selector@ for @performFetch:@
performFetchSelector :: Selector
performFetchSelector = mkSelector "performFetch:"

-- | @Selector@ for @deleteCacheWithName:@
deleteCacheWithNameSelector :: Selector
deleteCacheWithNameSelector = mkSelector "deleteCacheWithName:"

-- | @Selector@ for @sectionIndexTitleForSectionName:@
sectionIndexTitleForSectionNameSelector :: Selector
sectionIndexTitleForSectionNameSelector = mkSelector "sectionIndexTitleForSectionName:"

-- | @Selector@ for @sectionForSectionIndexTitle:atIndex:@
sectionForSectionIndexTitle_atIndexSelector :: Selector
sectionForSectionIndexTitle_atIndexSelector = mkSelector "sectionForSectionIndexTitle:atIndex:"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @sectionNameKeyPath@
sectionNameKeyPathSelector :: Selector
sectionNameKeyPathSelector = mkSelector "sectionNameKeyPath"

-- | @Selector@ for @cacheName@
cacheNameSelector :: Selector
cacheNameSelector = mkSelector "cacheName"

-- | @Selector@ for @fetchedObjects@
fetchedObjectsSelector :: Selector
fetchedObjectsSelector = mkSelector "fetchedObjects"

-- | @Selector@ for @sectionIndexTitles@
sectionIndexTitlesSelector :: Selector
sectionIndexTitlesSelector = mkSelector "sectionIndexTitles"

