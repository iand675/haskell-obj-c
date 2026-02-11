{-# LANGUAGE PatternSynonyms #-}
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
  , configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector
  , persistentStoreTypeForFileTypeSelector
  , writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector
  , readFromURL_ofType_errorSelector
  , revertToContentsOfURL_ofType_errorSelector
  , configurePersistentStoreCoordinatorForURL_ofType_errorSelector
  , managedObjectContextSelector
  , setManagedObjectContextSelector
  , managedObjectModelSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:@
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL url, IsNSString fileType, IsNSString configuration, IsNSDictionary storeOptions, IsNSError error_) => nsPersistentDocument -> url -> fileType -> configuration -> storeOptions -> error_ -> IO Bool
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error nsPersistentDocument  url fileType configuration storeOptions error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr fileType $ \raw_fileType ->
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr storeOptions $ \raw_storeOptions ->
        withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentDocument (mkSelector "configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_fileType :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_storeOptions :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- persistentStoreTypeForFileType:@
persistentStoreTypeForFileType :: (IsNSPersistentDocument nsPersistentDocument, IsNSString fileType) => nsPersistentDocument -> fileType -> IO (Id NSString)
persistentStoreTypeForFileType nsPersistentDocument  fileType =
withObjCPtr fileType $ \raw_fileType ->
    sendMsg nsPersistentDocument (mkSelector "persistentStoreTypeForFileType:") (retPtr retVoid) [argPtr (castPtr raw_fileType :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL absoluteURL, IsNSString typeName, IsNSURL absoluteOriginalContentsURL, IsNSError error_) => nsPersistentDocument -> absoluteURL -> typeName -> NSSaveOperationType -> absoluteOriginalContentsURL -> error_ -> IO Bool
writeToURL_ofType_forSaveOperation_originalContentsURL_error nsPersistentDocument  absoluteURL typeName saveOperation absoluteOriginalContentsURL error_ =
withObjCPtr absoluteURL $ \raw_absoluteURL ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr absoluteOriginalContentsURL $ \raw_absoluteOriginalContentsURL ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentDocument (mkSelector "writeToURL:ofType:forSaveOperation:originalContentsURL:error:") retCULong [argPtr (castPtr raw_absoluteURL :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr raw_absoluteOriginalContentsURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- readFromURL:ofType:error:@
readFromURL_ofType_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL absoluteURL, IsNSString typeName, IsNSError error_) => nsPersistentDocument -> absoluteURL -> typeName -> error_ -> IO Bool
readFromURL_ofType_error nsPersistentDocument  absoluteURL typeName error_ =
withObjCPtr absoluteURL $ \raw_absoluteURL ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentDocument (mkSelector "readFromURL:ofType:error:") retCULong [argPtr (castPtr raw_absoluteURL :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL inAbsoluteURL, IsNSString inTypeName, IsNSError outError) => nsPersistentDocument -> inAbsoluteURL -> inTypeName -> outError -> IO Bool
revertToContentsOfURL_ofType_error nsPersistentDocument  inAbsoluteURL inTypeName outError =
withObjCPtr inAbsoluteURL $ \raw_inAbsoluteURL ->
  withObjCPtr inTypeName $ \raw_inTypeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentDocument (mkSelector "revertToContentsOfURL:ofType:error:") retCULong [argPtr (castPtr raw_inAbsoluteURL :: Ptr ()), argPtr (castPtr raw_inTypeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- configurePersistentStoreCoordinatorForURL:ofType:error:@
configurePersistentStoreCoordinatorForURL_ofType_error :: (IsNSPersistentDocument nsPersistentDocument, IsNSURL url, IsNSString fileType, IsNSError error_) => nsPersistentDocument -> url -> fileType -> error_ -> IO Bool
configurePersistentStoreCoordinatorForURL_ofType_error nsPersistentDocument  url fileType error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr fileType $ \raw_fileType ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentDocument (mkSelector "configurePersistentStoreCoordinatorForURL:ofType:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_fileType :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- managedObjectContext@
managedObjectContext :: IsNSPersistentDocument nsPersistentDocument => nsPersistentDocument -> IO (Id NSManagedObjectContext)
managedObjectContext nsPersistentDocument  =
  sendMsg nsPersistentDocument (mkSelector "managedObjectContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManagedObjectContext:@
setManagedObjectContext :: (IsNSPersistentDocument nsPersistentDocument, IsNSManagedObjectContext value) => nsPersistentDocument -> value -> IO ()
setManagedObjectContext nsPersistentDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentDocument (mkSelector "setManagedObjectContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- managedObjectModel@
managedObjectModel :: IsNSPersistentDocument nsPersistentDocument => nsPersistentDocument -> IO (Id NSManagedObjectModel)
managedObjectModel nsPersistentDocument  =
  sendMsg nsPersistentDocument (mkSelector "managedObjectModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:@
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector :: Selector
configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_errorSelector = mkSelector "configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:"

-- | @Selector@ for @persistentStoreTypeForFileType:@
persistentStoreTypeForFileTypeSelector :: Selector
persistentStoreTypeForFileTypeSelector = mkSelector "persistentStoreTypeForFileType:"

-- | @Selector@ for @writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector :: Selector
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector = mkSelector "writeToURL:ofType:forSaveOperation:originalContentsURL:error:"

-- | @Selector@ for @readFromURL:ofType:error:@
readFromURL_ofType_errorSelector :: Selector
readFromURL_ofType_errorSelector = mkSelector "readFromURL:ofType:error:"

-- | @Selector@ for @revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_errorSelector :: Selector
revertToContentsOfURL_ofType_errorSelector = mkSelector "revertToContentsOfURL:ofType:error:"

-- | @Selector@ for @configurePersistentStoreCoordinatorForURL:ofType:error:@
configurePersistentStoreCoordinatorForURL_ofType_errorSelector :: Selector
configurePersistentStoreCoordinatorForURL_ofType_errorSelector = mkSelector "configurePersistentStoreCoordinatorForURL:ofType:error:"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @setManagedObjectContext:@
setManagedObjectContextSelector :: Selector
setManagedObjectContextSelector = mkSelector "setManagedObjectContext:"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector
managedObjectModelSelector = mkSelector "managedObjectModel"

