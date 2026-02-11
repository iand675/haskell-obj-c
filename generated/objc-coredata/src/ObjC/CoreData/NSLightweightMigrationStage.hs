{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLightweightMigrationStage@.
module ObjC.CoreData.NSLightweightMigrationStage
  ( NSLightweightMigrationStage
  , IsNSLightweightMigrationStage(..)
  , init_
  , initWithVersionChecksums
  , versionChecksums
  , initSelector
  , initWithVersionChecksumsSelector
  , versionChecksumsSelector


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
init_ :: IsNSLightweightMigrationStage nsLightweightMigrationStage => nsLightweightMigrationStage -> IO (Id NSLightweightMigrationStage)
init_ nsLightweightMigrationStage  =
  sendMsg nsLightweightMigrationStage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithVersionChecksums:@
initWithVersionChecksums :: (IsNSLightweightMigrationStage nsLightweightMigrationStage, IsNSArray versionChecksums) => nsLightweightMigrationStage -> versionChecksums -> IO (Id NSLightweightMigrationStage)
initWithVersionChecksums nsLightweightMigrationStage  versionChecksums =
withObjCPtr versionChecksums $ \raw_versionChecksums ->
    sendMsg nsLightweightMigrationStage (mkSelector "initWithVersionChecksums:") (retPtr retVoid) [argPtr (castPtr raw_versionChecksums :: Ptr ())] >>= ownedObject . castPtr

-- | @- versionChecksums@
versionChecksums :: IsNSLightweightMigrationStage nsLightweightMigrationStage => nsLightweightMigrationStage -> IO (Id NSArray)
versionChecksums nsLightweightMigrationStage  =
  sendMsg nsLightweightMigrationStage (mkSelector "versionChecksums") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVersionChecksums:@
initWithVersionChecksumsSelector :: Selector
initWithVersionChecksumsSelector = mkSelector "initWithVersionChecksums:"

-- | @Selector@ for @versionChecksums@
versionChecksumsSelector :: Selector
versionChecksumsSelector = mkSelector "versionChecksums"

