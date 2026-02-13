{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStagedMigrationManager@.
module ObjC.CoreData.NSStagedMigrationManager
  ( NSStagedMigrationManager
  , IsNSStagedMigrationManager(..)
  , init_
  , initWithMigrationStages
  , stages
  , container
  , containerSelector
  , initSelector
  , initWithMigrationStagesSelector
  , stagesSelector


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
init_ :: IsNSStagedMigrationManager nsStagedMigrationManager => nsStagedMigrationManager -> IO (Id NSStagedMigrationManager)
init_ nsStagedMigrationManager =
  sendOwnedMessage nsStagedMigrationManager initSelector

-- | @- initWithMigrationStages:@
initWithMigrationStages :: (IsNSStagedMigrationManager nsStagedMigrationManager, IsNSArray stages) => nsStagedMigrationManager -> stages -> IO (Id NSStagedMigrationManager)
initWithMigrationStages nsStagedMigrationManager stages =
  sendOwnedMessage nsStagedMigrationManager initWithMigrationStagesSelector (toNSArray stages)

-- | @- stages@
stages :: IsNSStagedMigrationManager nsStagedMigrationManager => nsStagedMigrationManager -> IO (Id NSArray)
stages nsStagedMigrationManager =
  sendMessage nsStagedMigrationManager stagesSelector

-- | @- container@
container :: IsNSStagedMigrationManager nsStagedMigrationManager => nsStagedMigrationManager -> IO (Id NSPersistentContainer)
container nsStagedMigrationManager =
  sendMessage nsStagedMigrationManager containerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSStagedMigrationManager)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMigrationStages:@
initWithMigrationStagesSelector :: Selector '[Id NSArray] (Id NSStagedMigrationManager)
initWithMigrationStagesSelector = mkSelector "initWithMigrationStages:"

-- | @Selector@ for @stages@
stagesSelector :: Selector '[] (Id NSArray)
stagesSelector = mkSelector "stages"

-- | @Selector@ for @container@
containerSelector :: Selector '[] (Id NSPersistentContainer)
containerSelector = mkSelector "container"

