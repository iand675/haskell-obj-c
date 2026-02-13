{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCustomMigrationStage@.
module ObjC.CoreData.NSCustomMigrationStage
  ( NSCustomMigrationStage
  , IsNSCustomMigrationStage(..)
  , init_
  , initWithCurrentModelReference_nextModelReference
  , currentModel
  , nextModel
  , willMigrateHandler
  , setWillMigrateHandler
  , didMigrateHandler
  , setDidMigrateHandler
  , currentModelSelector
  , didMigrateHandlerSelector
  , initSelector
  , initWithCurrentModelReference_nextModelReferenceSelector
  , nextModelSelector
  , setDidMigrateHandlerSelector
  , setWillMigrateHandlerSelector
  , willMigrateHandlerSelector


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
init_ :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Id NSCustomMigrationStage)
init_ nsCustomMigrationStage =
  sendOwnedMessage nsCustomMigrationStage initSelector

-- | @- initWithCurrentModelReference:nextModelReference:@
initWithCurrentModelReference_nextModelReference :: (IsNSCustomMigrationStage nsCustomMigrationStage, IsNSManagedObjectModelReference currentModel, IsNSManagedObjectModelReference nextModel) => nsCustomMigrationStage -> currentModel -> nextModel -> IO (Id NSCustomMigrationStage)
initWithCurrentModelReference_nextModelReference nsCustomMigrationStage currentModel nextModel =
  sendOwnedMessage nsCustomMigrationStage initWithCurrentModelReference_nextModelReferenceSelector (toNSManagedObjectModelReference currentModel) (toNSManagedObjectModelReference nextModel)

-- | @- currentModel@
currentModel :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Id NSManagedObjectModelReference)
currentModel nsCustomMigrationStage =
  sendMessage nsCustomMigrationStage currentModelSelector

-- | @- nextModel@
nextModel :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Id NSManagedObjectModelReference)
nextModel nsCustomMigrationStage =
  sendMessage nsCustomMigrationStage nextModelSelector

-- | @- willMigrateHandler@
willMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Ptr ())
willMigrateHandler nsCustomMigrationStage =
  sendMessage nsCustomMigrationStage willMigrateHandlerSelector

-- | @- setWillMigrateHandler:@
setWillMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> Ptr () -> IO ()
setWillMigrateHandler nsCustomMigrationStage value =
  sendMessage nsCustomMigrationStage setWillMigrateHandlerSelector value

-- | @- didMigrateHandler@
didMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Ptr ())
didMigrateHandler nsCustomMigrationStage =
  sendMessage nsCustomMigrationStage didMigrateHandlerSelector

-- | @- setDidMigrateHandler:@
setDidMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> Ptr () -> IO ()
setDidMigrateHandler nsCustomMigrationStage value =
  sendMessage nsCustomMigrationStage setDidMigrateHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCustomMigrationStage)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCurrentModelReference:nextModelReference:@
initWithCurrentModelReference_nextModelReferenceSelector :: Selector '[Id NSManagedObjectModelReference, Id NSManagedObjectModelReference] (Id NSCustomMigrationStage)
initWithCurrentModelReference_nextModelReferenceSelector = mkSelector "initWithCurrentModelReference:nextModelReference:"

-- | @Selector@ for @currentModel@
currentModelSelector :: Selector '[] (Id NSManagedObjectModelReference)
currentModelSelector = mkSelector "currentModel"

-- | @Selector@ for @nextModel@
nextModelSelector :: Selector '[] (Id NSManagedObjectModelReference)
nextModelSelector = mkSelector "nextModel"

-- | @Selector@ for @willMigrateHandler@
willMigrateHandlerSelector :: Selector '[] (Ptr ())
willMigrateHandlerSelector = mkSelector "willMigrateHandler"

-- | @Selector@ for @setWillMigrateHandler:@
setWillMigrateHandlerSelector :: Selector '[Ptr ()] ()
setWillMigrateHandlerSelector = mkSelector "setWillMigrateHandler:"

-- | @Selector@ for @didMigrateHandler@
didMigrateHandlerSelector :: Selector '[] (Ptr ())
didMigrateHandlerSelector = mkSelector "didMigrateHandler"

-- | @Selector@ for @setDidMigrateHandler:@
setDidMigrateHandlerSelector :: Selector '[Ptr ()] ()
setDidMigrateHandlerSelector = mkSelector "setDidMigrateHandler:"

