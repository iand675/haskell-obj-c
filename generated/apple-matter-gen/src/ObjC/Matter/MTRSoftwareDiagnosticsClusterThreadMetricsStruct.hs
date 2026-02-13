{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSoftwareDiagnosticsClusterThreadMetricsStruct@.
module ObjC.Matter.MTRSoftwareDiagnosticsClusterThreadMetricsStruct
  ( MTRSoftwareDiagnosticsClusterThreadMetricsStruct
  , IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct(..)
  , id_
  , setId
  , name
  , setName
  , stackFreeCurrent
  , setStackFreeCurrent
  , stackFreeMinimum
  , setStackFreeMinimum
  , stackSize
  , setStackSize
  , idSelector
  , nameSelector
  , setIdSelector
  , setNameSelector
  , setStackFreeCurrentSelector
  , setStackFreeMinimumSelector
  , setStackSizeSelector
  , stackFreeCurrentSelector
  , stackFreeMinimumSelector
  , stackSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- id@
id_ :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
id_ mtrSoftwareDiagnosticsClusterThreadMetricsStruct =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct idSelector

-- | @- setId:@
setId :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setId mtrSoftwareDiagnosticsClusterThreadMetricsStruct value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct setIdSelector (toNSNumber value)

-- | @- name@
name :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSString)
name mtrSoftwareDiagnosticsClusterThreadMetricsStruct =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct nameSelector

-- | @- setName:@
setName :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSString value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setName mtrSoftwareDiagnosticsClusterThreadMetricsStruct value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct setNameSelector (toNSString value)

-- | @- stackFreeCurrent@
stackFreeCurrent :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
stackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetricsStruct =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct stackFreeCurrentSelector

-- | @- setStackFreeCurrent:@
setStackFreeCurrent :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setStackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetricsStruct value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct setStackFreeCurrentSelector (toNSNumber value)

-- | @- stackFreeMinimum@
stackFreeMinimum :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
stackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetricsStruct =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct stackFreeMinimumSelector

-- | @- setStackFreeMinimum:@
setStackFreeMinimum :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setStackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetricsStruct value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct setStackFreeMinimumSelector (toNSNumber value)

-- | @- stackSize@
stackSize :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
stackSize mtrSoftwareDiagnosticsClusterThreadMetricsStruct =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct stackSizeSelector

-- | @- setStackSize:@
setStackSize :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setStackSize mtrSoftwareDiagnosticsClusterThreadMetricsStruct value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetricsStruct setStackSizeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector '[] (Id NSNumber)
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector '[Id NSNumber] ()
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stackFreeCurrent@
stackFreeCurrentSelector :: Selector '[] (Id NSNumber)
stackFreeCurrentSelector = mkSelector "stackFreeCurrent"

-- | @Selector@ for @setStackFreeCurrent:@
setStackFreeCurrentSelector :: Selector '[Id NSNumber] ()
setStackFreeCurrentSelector = mkSelector "setStackFreeCurrent:"

-- | @Selector@ for @stackFreeMinimum@
stackFreeMinimumSelector :: Selector '[] (Id NSNumber)
stackFreeMinimumSelector = mkSelector "stackFreeMinimum"

-- | @Selector@ for @setStackFreeMinimum:@
setStackFreeMinimumSelector :: Selector '[Id NSNumber] ()
setStackFreeMinimumSelector = mkSelector "setStackFreeMinimum:"

-- | @Selector@ for @stackSize@
stackSizeSelector :: Selector '[] (Id NSNumber)
stackSizeSelector = mkSelector "stackSize"

-- | @Selector@ for @setStackSize:@
setStackSizeSelector :: Selector '[Id NSNumber] ()
setStackSizeSelector = mkSelector "setStackSize:"

