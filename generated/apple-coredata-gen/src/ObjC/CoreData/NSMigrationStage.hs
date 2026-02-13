{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMigrationStage@.
module ObjC.CoreData.NSMigrationStage
  ( NSMigrationStage
  , IsNSMigrationStage(..)
  , label
  , setLabel
  , labelSelector
  , setLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- label@
label :: IsNSMigrationStage nsMigrationStage => nsMigrationStage -> IO (Id NSString)
label nsMigrationStage =
  sendMessage nsMigrationStage labelSelector

-- | @- setLabel:@
setLabel :: (IsNSMigrationStage nsMigrationStage, IsNSString value) => nsMigrationStage -> value -> IO ()
setLabel nsMigrationStage value =
  sendMessage nsMigrationStage setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

