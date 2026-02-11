{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSManagedObjectModelReference@.
module ObjC.CoreData.NSManagedObjectModelReference
  ( NSManagedObjectModelReference
  , IsNSManagedObjectModelReference(..)
  , init_
  , initWithModel_versionChecksum
  , initWithFileURL_versionChecksum
  , initWithEntityVersionHashes_inBundle_versionChecksum
  , initWithName_inBundle_versionChecksum
  , resolvedModel
  , versionChecksum
  , initSelector
  , initWithModel_versionChecksumSelector
  , initWithFileURL_versionChecksumSelector
  , initWithEntityVersionHashes_inBundle_versionChecksumSelector
  , initWithName_inBundle_versionChecksumSelector
  , resolvedModelSelector
  , versionChecksumSelector


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

-- | @- init@
init_ :: IsNSManagedObjectModelReference nsManagedObjectModelReference => nsManagedObjectModelReference -> IO (Id NSManagedObjectModelReference)
init_ nsManagedObjectModelReference  =
  sendMsg nsManagedObjectModelReference (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithModel:versionChecksum:@
initWithModel_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSManagedObjectModel model, IsNSString versionChecksum) => nsManagedObjectModelReference -> model -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithModel_versionChecksum nsManagedObjectModelReference  model versionChecksum =
withObjCPtr model $ \raw_model ->
  withObjCPtr versionChecksum $ \raw_versionChecksum ->
      sendMsg nsManagedObjectModelReference (mkSelector "initWithModel:versionChecksum:") (retPtr retVoid) [argPtr (castPtr raw_model :: Ptr ()), argPtr (castPtr raw_versionChecksum :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFileURL:versionChecksum:@
initWithFileURL_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSURL fileURL, IsNSString versionChecksum) => nsManagedObjectModelReference -> fileURL -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithFileURL_versionChecksum nsManagedObjectModelReference  fileURL versionChecksum =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr versionChecksum $ \raw_versionChecksum ->
      sendMsg nsManagedObjectModelReference (mkSelector "initWithFileURL:versionChecksum:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_versionChecksum :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEntityVersionHashes:inBundle:versionChecksum:@
initWithEntityVersionHashes_inBundle_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSDictionary versionHash, IsNSBundle bundle, IsNSString versionChecksum) => nsManagedObjectModelReference -> versionHash -> bundle -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithEntityVersionHashes_inBundle_versionChecksum nsManagedObjectModelReference  versionHash bundle versionChecksum =
withObjCPtr versionHash $ \raw_versionHash ->
  withObjCPtr bundle $ \raw_bundle ->
    withObjCPtr versionChecksum $ \raw_versionChecksum ->
        sendMsg nsManagedObjectModelReference (mkSelector "initWithEntityVersionHashes:inBundle:versionChecksum:") (retPtr retVoid) [argPtr (castPtr raw_versionHash :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ()), argPtr (castPtr raw_versionChecksum :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:inBundle:versionChecksum:@
initWithName_inBundle_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSString modelName, IsNSBundle bundle, IsNSString versionChecksum) => nsManagedObjectModelReference -> modelName -> bundle -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithName_inBundle_versionChecksum nsManagedObjectModelReference  modelName bundle versionChecksum =
withObjCPtr modelName $ \raw_modelName ->
  withObjCPtr bundle $ \raw_bundle ->
    withObjCPtr versionChecksum $ \raw_versionChecksum ->
        sendMsg nsManagedObjectModelReference (mkSelector "initWithName:inBundle:versionChecksum:") (retPtr retVoid) [argPtr (castPtr raw_modelName :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ()), argPtr (castPtr raw_versionChecksum :: Ptr ())] >>= ownedObject . castPtr

-- | @- resolvedModel@
resolvedModel :: IsNSManagedObjectModelReference nsManagedObjectModelReference => nsManagedObjectModelReference -> IO (Id NSManagedObjectModel)
resolvedModel nsManagedObjectModelReference  =
  sendMsg nsManagedObjectModelReference (mkSelector "resolvedModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- versionChecksum@
versionChecksum :: IsNSManagedObjectModelReference nsManagedObjectModelReference => nsManagedObjectModelReference -> IO (Id NSString)
versionChecksum nsManagedObjectModelReference  =
  sendMsg nsManagedObjectModelReference (mkSelector "versionChecksum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithModel:versionChecksum:@
initWithModel_versionChecksumSelector :: Selector
initWithModel_versionChecksumSelector = mkSelector "initWithModel:versionChecksum:"

-- | @Selector@ for @initWithFileURL:versionChecksum:@
initWithFileURL_versionChecksumSelector :: Selector
initWithFileURL_versionChecksumSelector = mkSelector "initWithFileURL:versionChecksum:"

-- | @Selector@ for @initWithEntityVersionHashes:inBundle:versionChecksum:@
initWithEntityVersionHashes_inBundle_versionChecksumSelector :: Selector
initWithEntityVersionHashes_inBundle_versionChecksumSelector = mkSelector "initWithEntityVersionHashes:inBundle:versionChecksum:"

-- | @Selector@ for @initWithName:inBundle:versionChecksum:@
initWithName_inBundle_versionChecksumSelector :: Selector
initWithName_inBundle_versionChecksumSelector = mkSelector "initWithName:inBundle:versionChecksum:"

-- | @Selector@ for @resolvedModel@
resolvedModelSelector :: Selector
resolvedModelSelector = mkSelector "resolvedModel"

-- | @Selector@ for @versionChecksum@
versionChecksumSelector :: Selector
versionChecksumSelector = mkSelector "versionChecksum"

