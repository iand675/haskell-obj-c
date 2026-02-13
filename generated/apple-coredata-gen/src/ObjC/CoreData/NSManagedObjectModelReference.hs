{-# LANGUAGE DataKinds #-}
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
  , initWithEntityVersionHashes_inBundle_versionChecksumSelector
  , initWithFileURL_versionChecksumSelector
  , initWithModel_versionChecksumSelector
  , initWithName_inBundle_versionChecksumSelector
  , resolvedModelSelector
  , versionChecksumSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSManagedObjectModelReference nsManagedObjectModelReference => nsManagedObjectModelReference -> IO (Id NSManagedObjectModelReference)
init_ nsManagedObjectModelReference =
  sendOwnedMessage nsManagedObjectModelReference initSelector

-- | @- initWithModel:versionChecksum:@
initWithModel_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSManagedObjectModel model, IsNSString versionChecksum) => nsManagedObjectModelReference -> model -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithModel_versionChecksum nsManagedObjectModelReference model versionChecksum =
  sendOwnedMessage nsManagedObjectModelReference initWithModel_versionChecksumSelector (toNSManagedObjectModel model) (toNSString versionChecksum)

-- | @- initWithFileURL:versionChecksum:@
initWithFileURL_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSURL fileURL, IsNSString versionChecksum) => nsManagedObjectModelReference -> fileURL -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithFileURL_versionChecksum nsManagedObjectModelReference fileURL versionChecksum =
  sendOwnedMessage nsManagedObjectModelReference initWithFileURL_versionChecksumSelector (toNSURL fileURL) (toNSString versionChecksum)

-- | @- initWithEntityVersionHashes:inBundle:versionChecksum:@
initWithEntityVersionHashes_inBundle_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSDictionary versionHash, IsNSBundle bundle, IsNSString versionChecksum) => nsManagedObjectModelReference -> versionHash -> bundle -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithEntityVersionHashes_inBundle_versionChecksum nsManagedObjectModelReference versionHash bundle versionChecksum =
  sendOwnedMessage nsManagedObjectModelReference initWithEntityVersionHashes_inBundle_versionChecksumSelector (toNSDictionary versionHash) (toNSBundle bundle) (toNSString versionChecksum)

-- | @- initWithName:inBundle:versionChecksum:@
initWithName_inBundle_versionChecksum :: (IsNSManagedObjectModelReference nsManagedObjectModelReference, IsNSString modelName, IsNSBundle bundle, IsNSString versionChecksum) => nsManagedObjectModelReference -> modelName -> bundle -> versionChecksum -> IO (Id NSManagedObjectModelReference)
initWithName_inBundle_versionChecksum nsManagedObjectModelReference modelName bundle versionChecksum =
  sendOwnedMessage nsManagedObjectModelReference initWithName_inBundle_versionChecksumSelector (toNSString modelName) (toNSBundle bundle) (toNSString versionChecksum)

-- | @- resolvedModel@
resolvedModel :: IsNSManagedObjectModelReference nsManagedObjectModelReference => nsManagedObjectModelReference -> IO (Id NSManagedObjectModel)
resolvedModel nsManagedObjectModelReference =
  sendMessage nsManagedObjectModelReference resolvedModelSelector

-- | @- versionChecksum@
versionChecksum :: IsNSManagedObjectModelReference nsManagedObjectModelReference => nsManagedObjectModelReference -> IO (Id NSString)
versionChecksum nsManagedObjectModelReference =
  sendMessage nsManagedObjectModelReference versionChecksumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSManagedObjectModelReference)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithModel:versionChecksum:@
initWithModel_versionChecksumSelector :: Selector '[Id NSManagedObjectModel, Id NSString] (Id NSManagedObjectModelReference)
initWithModel_versionChecksumSelector = mkSelector "initWithModel:versionChecksum:"

-- | @Selector@ for @initWithFileURL:versionChecksum:@
initWithFileURL_versionChecksumSelector :: Selector '[Id NSURL, Id NSString] (Id NSManagedObjectModelReference)
initWithFileURL_versionChecksumSelector = mkSelector "initWithFileURL:versionChecksum:"

-- | @Selector@ for @initWithEntityVersionHashes:inBundle:versionChecksum:@
initWithEntityVersionHashes_inBundle_versionChecksumSelector :: Selector '[Id NSDictionary, Id NSBundle, Id NSString] (Id NSManagedObjectModelReference)
initWithEntityVersionHashes_inBundle_versionChecksumSelector = mkSelector "initWithEntityVersionHashes:inBundle:versionChecksum:"

-- | @Selector@ for @initWithName:inBundle:versionChecksum:@
initWithName_inBundle_versionChecksumSelector :: Selector '[Id NSString, Id NSBundle, Id NSString] (Id NSManagedObjectModelReference)
initWithName_inBundle_versionChecksumSelector = mkSelector "initWithName:inBundle:versionChecksum:"

-- | @Selector@ for @resolvedModel@
resolvedModelSelector :: Selector '[] (Id NSManagedObjectModel)
resolvedModelSelector = mkSelector "resolvedModel"

-- | @Selector@ for @versionChecksum@
versionChecksumSelector :: Selector '[] (Id NSString)
versionChecksumSelector = mkSelector "versionChecksum"

