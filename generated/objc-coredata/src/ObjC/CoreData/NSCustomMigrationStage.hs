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
  , initSelector
  , initWithCurrentModelReference_nextModelReferenceSelector
  , currentModelSelector
  , nextModelSelector
  , willMigrateHandlerSelector
  , setWillMigrateHandlerSelector
  , didMigrateHandlerSelector
  , setDidMigrateHandlerSelector


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
init_ :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Id NSCustomMigrationStage)
init_ nsCustomMigrationStage  =
  sendMsg nsCustomMigrationStage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCurrentModelReference:nextModelReference:@
initWithCurrentModelReference_nextModelReference :: (IsNSCustomMigrationStage nsCustomMigrationStage, IsNSManagedObjectModelReference currentModel, IsNSManagedObjectModelReference nextModel) => nsCustomMigrationStage -> currentModel -> nextModel -> IO (Id NSCustomMigrationStage)
initWithCurrentModelReference_nextModelReference nsCustomMigrationStage  currentModel nextModel =
withObjCPtr currentModel $ \raw_currentModel ->
  withObjCPtr nextModel $ \raw_nextModel ->
      sendMsg nsCustomMigrationStage (mkSelector "initWithCurrentModelReference:nextModelReference:") (retPtr retVoid) [argPtr (castPtr raw_currentModel :: Ptr ()), argPtr (castPtr raw_nextModel :: Ptr ())] >>= ownedObject . castPtr

-- | @- currentModel@
currentModel :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Id NSManagedObjectModelReference)
currentModel nsCustomMigrationStage  =
  sendMsg nsCustomMigrationStage (mkSelector "currentModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextModel@
nextModel :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Id NSManagedObjectModelReference)
nextModel nsCustomMigrationStage  =
  sendMsg nsCustomMigrationStage (mkSelector "nextModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- willMigrateHandler@
willMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Ptr ())
willMigrateHandler nsCustomMigrationStage  =
  fmap castPtr $ sendMsg nsCustomMigrationStage (mkSelector "willMigrateHandler") (retPtr retVoid) []

-- | @- setWillMigrateHandler:@
setWillMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> Ptr () -> IO ()
setWillMigrateHandler nsCustomMigrationStage  value =
  sendMsg nsCustomMigrationStage (mkSelector "setWillMigrateHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- didMigrateHandler@
didMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> IO (Ptr ())
didMigrateHandler nsCustomMigrationStage  =
  fmap castPtr $ sendMsg nsCustomMigrationStage (mkSelector "didMigrateHandler") (retPtr retVoid) []

-- | @- setDidMigrateHandler:@
setDidMigrateHandler :: IsNSCustomMigrationStage nsCustomMigrationStage => nsCustomMigrationStage -> Ptr () -> IO ()
setDidMigrateHandler nsCustomMigrationStage  value =
  sendMsg nsCustomMigrationStage (mkSelector "setDidMigrateHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCurrentModelReference:nextModelReference:@
initWithCurrentModelReference_nextModelReferenceSelector :: Selector
initWithCurrentModelReference_nextModelReferenceSelector = mkSelector "initWithCurrentModelReference:nextModelReference:"

-- | @Selector@ for @currentModel@
currentModelSelector :: Selector
currentModelSelector = mkSelector "currentModel"

-- | @Selector@ for @nextModel@
nextModelSelector :: Selector
nextModelSelector = mkSelector "nextModel"

-- | @Selector@ for @willMigrateHandler@
willMigrateHandlerSelector :: Selector
willMigrateHandlerSelector = mkSelector "willMigrateHandler"

-- | @Selector@ for @setWillMigrateHandler:@
setWillMigrateHandlerSelector :: Selector
setWillMigrateHandlerSelector = mkSelector "setWillMigrateHandler:"

-- | @Selector@ for @didMigrateHandler@
didMigrateHandlerSelector :: Selector
didMigrateHandlerSelector = mkSelector "didMigrateHandler"

-- | @Selector@ for @setDidMigrateHandler:@
setDidMigrateHandlerSelector :: Selector
setDidMigrateHandlerSelector = mkSelector "setDidMigrateHandler:"

