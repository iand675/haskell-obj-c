{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSoftwareDiagnosticsClusterThreadMetrics@.
module ObjC.Matter.MTRSoftwareDiagnosticsClusterThreadMetrics
  ( MTRSoftwareDiagnosticsClusterThreadMetrics
  , IsMTRSoftwareDiagnosticsClusterThreadMetrics(..)
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
id_ :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
id_ mtrSoftwareDiagnosticsClusterThreadMetrics =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics idSelector

-- | @- setId:@
setId :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setId mtrSoftwareDiagnosticsClusterThreadMetrics value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics setIdSelector (toNSNumber value)

-- | @- name@
name :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSString)
name mtrSoftwareDiagnosticsClusterThreadMetrics =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics nameSelector

-- | @- setName:@
setName :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSString value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setName mtrSoftwareDiagnosticsClusterThreadMetrics value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics setNameSelector (toNSString value)

-- | @- stackFreeCurrent@
stackFreeCurrent :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
stackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetrics =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics stackFreeCurrentSelector

-- | @- setStackFreeCurrent:@
setStackFreeCurrent :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setStackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetrics value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics setStackFreeCurrentSelector (toNSNumber value)

-- | @- stackFreeMinimum@
stackFreeMinimum :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
stackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetrics =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics stackFreeMinimumSelector

-- | @- setStackFreeMinimum:@
setStackFreeMinimum :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setStackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetrics value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics setStackFreeMinimumSelector (toNSNumber value)

-- | @- stackSize@
stackSize :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
stackSize mtrSoftwareDiagnosticsClusterThreadMetrics =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics stackSizeSelector

-- | @- setStackSize:@
setStackSize :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setStackSize mtrSoftwareDiagnosticsClusterThreadMetrics value =
  sendMessage mtrSoftwareDiagnosticsClusterThreadMetrics setStackSizeSelector (toNSNumber value)

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

