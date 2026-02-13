{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSLightweightMigrationStage nsLightweightMigrationStage => nsLightweightMigrationStage -> IO (Id NSLightweightMigrationStage)
init_ nsLightweightMigrationStage =
  sendOwnedMessage nsLightweightMigrationStage initSelector

-- | @- initWithVersionChecksums:@
initWithVersionChecksums :: (IsNSLightweightMigrationStage nsLightweightMigrationStage, IsNSArray versionChecksums) => nsLightweightMigrationStage -> versionChecksums -> IO (Id NSLightweightMigrationStage)
initWithVersionChecksums nsLightweightMigrationStage versionChecksums =
  sendOwnedMessage nsLightweightMigrationStage initWithVersionChecksumsSelector (toNSArray versionChecksums)

-- | @- versionChecksums@
versionChecksums :: IsNSLightweightMigrationStage nsLightweightMigrationStage => nsLightweightMigrationStage -> IO (Id NSArray)
versionChecksums nsLightweightMigrationStage =
  sendMessage nsLightweightMigrationStage versionChecksumsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSLightweightMigrationStage)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVersionChecksums:@
initWithVersionChecksumsSelector :: Selector '[Id NSArray] (Id NSLightweightMigrationStage)
initWithVersionChecksumsSelector = mkSelector "initWithVersionChecksums:"

-- | @Selector@ for @versionChecksums@
versionChecksumsSelector :: Selector '[] (Id NSArray)
versionChecksumsSelector = mkSelector "versionChecksums"

