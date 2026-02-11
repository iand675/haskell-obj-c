{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMergeConflict@.
module ObjC.CoreData.NSMergeConflict
  ( NSMergeConflict
  , IsNSMergeConflict(..)
  , initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot
  , init_
  , sourceObject
  , objectSnapshot
  , cachedSnapshot
  , persistedSnapshot
  , newVersionNumber
  , oldVersionNumber
  , initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector
  , initSelector
  , sourceObjectSelector
  , objectSnapshotSelector
  , cachedSnapshotSelector
  , persistedSnapshotSelector
  , newVersionNumberSelector
  , oldVersionNumberSelector


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

-- | @- initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:@
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot :: (IsNSMergeConflict nsMergeConflict, IsNSManagedObject srcObject, IsNSDictionary cachesnap, IsNSDictionary persnap) => nsMergeConflict -> srcObject -> CULong -> CULong -> cachesnap -> persnap -> IO (Id NSMergeConflict)
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot nsMergeConflict  srcObject newvers oldvers cachesnap persnap =
withObjCPtr srcObject $ \raw_srcObject ->
  withObjCPtr cachesnap $ \raw_cachesnap ->
    withObjCPtr persnap $ \raw_persnap ->
        sendMsg nsMergeConflict (mkSelector "initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:") (retPtr retVoid) [argPtr (castPtr raw_srcObject :: Ptr ()), argCULong (fromIntegral newvers), argCULong (fromIntegral oldvers), argPtr (castPtr raw_cachesnap :: Ptr ()), argPtr (castPtr raw_persnap :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSMergeConflict)
init_ nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- sourceObject@
sourceObject :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSManagedObject)
sourceObject nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "sourceObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectSnapshot@
objectSnapshot :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSDictionary)
objectSnapshot nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "objectSnapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cachedSnapshot@
cachedSnapshot :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSDictionary)
cachedSnapshot nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "cachedSnapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- persistedSnapshot@
persistedSnapshot :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO (Id NSDictionary)
persistedSnapshot nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "persistedSnapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- newVersionNumber@
newVersionNumber :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO CULong
newVersionNumber nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "newVersionNumber") retCULong []

-- | @- oldVersionNumber@
oldVersionNumber :: IsNSMergeConflict nsMergeConflict => nsMergeConflict -> IO CULong
oldVersionNumber nsMergeConflict  =
  sendMsg nsMergeConflict (mkSelector "oldVersionNumber") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:@
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector :: Selector
initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshotSelector = mkSelector "initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sourceObject@
sourceObjectSelector :: Selector
sourceObjectSelector = mkSelector "sourceObject"

-- | @Selector@ for @objectSnapshot@
objectSnapshotSelector :: Selector
objectSnapshotSelector = mkSelector "objectSnapshot"

-- | @Selector@ for @cachedSnapshot@
cachedSnapshotSelector :: Selector
cachedSnapshotSelector = mkSelector "cachedSnapshot"

-- | @Selector@ for @persistedSnapshot@
persistedSnapshotSelector :: Selector
persistedSnapshotSelector = mkSelector "persistedSnapshot"

-- | @Selector@ for @newVersionNumber@
newVersionNumberSelector :: Selector
newVersionNumberSelector = mkSelector "newVersionNumber"

-- | @Selector@ for @oldVersionNumber@
oldVersionNumberSelector :: Selector
oldVersionNumberSelector = mkSelector "oldVersionNumber"

