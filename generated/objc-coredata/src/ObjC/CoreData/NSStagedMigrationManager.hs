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
  , initSelector
  , initWithMigrationStagesSelector
  , stagesSelector
  , containerSelector


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
init_ :: IsNSStagedMigrationManager nsStagedMigrationManager => nsStagedMigrationManager -> IO (Id NSStagedMigrationManager)
init_ nsStagedMigrationManager  =
  sendMsg nsStagedMigrationManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithMigrationStages:@
initWithMigrationStages :: (IsNSStagedMigrationManager nsStagedMigrationManager, IsNSArray stages) => nsStagedMigrationManager -> stages -> IO (Id NSStagedMigrationManager)
initWithMigrationStages nsStagedMigrationManager  stages =
withObjCPtr stages $ \raw_stages ->
    sendMsg nsStagedMigrationManager (mkSelector "initWithMigrationStages:") (retPtr retVoid) [argPtr (castPtr raw_stages :: Ptr ())] >>= ownedObject . castPtr

-- | @- stages@
stages :: IsNSStagedMigrationManager nsStagedMigrationManager => nsStagedMigrationManager -> IO (Id NSArray)
stages nsStagedMigrationManager  =
  sendMsg nsStagedMigrationManager (mkSelector "stages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- container@
container :: IsNSStagedMigrationManager nsStagedMigrationManager => nsStagedMigrationManager -> IO (Id NSPersistentContainer)
container nsStagedMigrationManager  =
  sendMsg nsStagedMigrationManager (mkSelector "container") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMigrationStages:@
initWithMigrationStagesSelector :: Selector
initWithMigrationStagesSelector = mkSelector "initWithMigrationStages:"

-- | @Selector@ for @stages@
stagesSelector :: Selector
stagesSelector = mkSelector "stages"

-- | @Selector@ for @container@
containerSelector :: Selector
containerSelector = mkSelector "container"

