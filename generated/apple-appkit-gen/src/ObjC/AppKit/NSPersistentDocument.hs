{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentDocument@.
module ObjC.AppKit.NSPersistentDocument
  ( NSPersistentDocument
  , IsNSPersistentDocument(..)
  , configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error
  , persistentStoreTypeForFileType
  , writeToURL_ofType_forSaveOperation_originalContentsURL_error
  , readFromURL_ofType_error
  , revertToContentsOfURL_ofType_error
  , configurePersistentStoreCoordinatorForURL_ofType_error
  , managedObjectContext
  , setManagedObjectContext
  , managedObjectModel
  , configurePersistentStoreCoordinatorForURL_ofType_errorSelector
  , configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector
  , managedObjectContextSelector
  , managedObjectModelSelector
  , persistentStoreTypeForFileTypeSelector
  , readFromURL_ofType_errorSelector
  , revertToContentsOfURL_ofType_errorSelector
  , setManagedObjectContextSelector
  , writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector

  -- * Enum types
  , NSSaveOperationType(NSSaveOperationType)
  , pattern NSSaveOperation
  , pattern NSSaveAsOperation
  , pattern NSSaveToOperation
  , pattern NSAutosaveInPlaceOperation
  , pattern NSAutosaveElsewhereOperation
  , pattern NSAutosaveAsOperation
  , pattern NSAutosaveOperation

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:@
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL url, IsNSString fileType, IsNSString configuration, IsNSDictionary storeOptions, IsNSError error_) => nsPersistentDocument -> url -> fileType -> configuration -> storeOptions -> error_ -> IO Bool
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error nsPersistentDocument url fileType configuration storeOptions error_ =
  sendMessage nsPersistentDocument configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector (toNSURL url) (toNSString fileType) (toNSString configuration) (toNSDictionary storeOptions) (toNSError error_)

-- | @- persistentStoreTypeForFileType:@
persistentStoreTypeForFileType :: (IsNSPersistentDocument nsPersistentDocument, IsNSString fileType) => nsPersistentDocument -> fileType -> IO (Id NSString)
persistentStoreTypeForFileType nsPersistentDocument fileType =
  sendMessage nsPersistentDocument persistentStoreTypeForFileTypeSelector (toNSString fileType)

-- | @- writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL absoluteURL, IsNSString typeName, IsNSURL absoluteOriginalContentsURL, IsNSError error_) => nsPersistentDocument -> absoluteURL -> typeName -> NSSaveOperationType -> absoluteOriginalContentsURL -> error_ -> IO Bool
writeToURL_ofType_forSaveOperation_originalContentsURL_error nsPersistentDocument absoluteURL typeName saveOperation absoluteOriginalContentsURL error_ =
  sendMessage nsPersistentDocument writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector (toNSURL absoluteURL) (toNSString typeName) saveOperation (toNSURL absoluteOriginalContentsURL) (toNSError error_)

-- | @- readFromURL:ofType:error:@
readFromURL_ofType_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL absoluteURL, IsNSString typeName, IsNSError error_) => nsPersistentDocument -> absoluteURL -> typeName -> error_ -> IO Bool
readFromURL_ofType_error nsPersistentDocument absoluteURL typeName error_ =
  sendMessage nsPersistentDocument readFromURL_ofType_errorSelector (toNSURL absoluteURL) (toNSString typeName) (toNSError error_)

-- | @- revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL inAbsoluteURL, IsNSString inTypeName, IsNSError outError) => nsPersistentDocument -> inAbsoluteURL -> inTypeName -> outError -> IO Bool
revertToContentsOfURL_ofType_error nsPersistentDocument inAbsoluteURL inTypeName outError =
  sendMessage nsPersistentDocument revertToContentsOfURL_ofType_errorSelector (toNSURL inAbsoluteURL) (toNSString inTypeName) (toNSError outError)

-- | @- configurePersistentStoreCoordinatorForURL:ofType:error:@
configurePersistentStoreCoordinatorForURL_ofType_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL url, IsNSString fileType, IsNSError error_) => nsPersistentDocument -> url -> fileType -> error_ -> IO Bool
configurePersistentStoreCoordinatorForURL_ofType_error nsPersistentDocument url fileType error_ =
  sendMessage nsPersistentDocument configurePersistentStoreCoordinatorForURL_ofType_errorSelector (toNSURL url) (toNSString fileType) (toNSError error_)

-- | @- managedObjectContext@
managedObjectContext :: IsNSPersistentDocument nsPersistentDocument => nsPersistentDocument -> IO (Id NSManagedObjectContext)
managedObjectContext nsPersistentDocument =
  sendMessage nsPersistentDocument managedObjectContextSelector

-- | @- setManagedObjectContext:@
setManagedObjectContext :: (IsNSPersistentDocument nsPersistentDocument, IsNSManagedObjectContext value) => nsPersistentDocument -> value -> IO ()
setManagedObjectContext nsPersistentDocument value =
  sendMessage nsPersistentDocument setManagedObjectContextSelector (toNSManagedObjectContext value)

-- | @- managedObjectModel@
managedObjectModel :: IsNSPersistentDocument nsPersistentDocument => nsPersistentDocument -> IO (Id NSManagedObjectModel)
managedObjectModel nsPersistentDocument =
  sendMessage nsPersistentDocument managedObjectModelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:@
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSString, Id NSDictionary, Id NSError] Bool
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector = mkSelector "configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:"

-- | @Selector@ for @persistentStoreTypeForFileType:@
persistentStoreTypeForFileTypeSelector :: Selector '[Id NSString] (Id NSString)
persistentStoreTypeForFileTypeSelector = mkSelector "persistentStoreTypeForFileType:"

-- | @Selector@ for @writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, Id NSURL, Id NSError] Bool
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector = mkSelector "writeToURL:ofType:forSaveOperation:originalContentsURL:error:"

-- | @Selector@ for @readFromURL:ofType:error:@
readFromURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] Bool
readFromURL_ofType_errorSelector = mkSelector "readFromURL:ofType:error:"

-- | @Selector@ for @revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] Bool
revertToContentsOfURL_ofType_errorSelector = mkSelector "revertToContentsOfURL:ofType:error:"

-- | @Selector@ for @configurePersistentStoreCoordinatorForURL:ofType:error:@
configurePersistentStoreCoordinatorForURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] Bool
configurePersistentStoreCoordinatorForURL_ofType_errorSelector = mkSelector "configurePersistentStoreCoordinatorForURL:ofType:error:"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector '[] (Id NSManagedObjectContext)
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @setManagedObjectContext:@
setManagedObjectContextSelector :: Selector '[Id NSManagedObjectContext] ()
setManagedObjectContextSelector = mkSelector "setManagedObjectContext:"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector '[] (Id NSManagedObjectModel)
managedObjectModelSelector = mkSelector "managedObjectModel"

