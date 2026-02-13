{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent
  ( MTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent
  , IsMTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent(..)
  , current
  , setCurrent
  , previous
  , setPrevious
  , currentSelector
  , previousSelector
  , setCurrentSelector
  , setPreviousSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- current@
current :: IsMTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent => mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent -> IO (Id NSArray)
current mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent =
  sendMessage mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent, IsNSArray value) => mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent -> value -> IO ()
setCurrent mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent => mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent -> IO (Id NSArray)
previous mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent =
  sendMessage mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent, IsNSArray value) => mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent -> value -> IO ()
setPrevious mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNetworkFaultChangeEvent setPreviousSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @current@
currentSelector :: Selector '[] (Id NSArray)
currentSelector = mkSelector "current"

-- | @Selector@ for @setCurrent:@
setCurrentSelector :: Selector '[Id NSArray] ()
setCurrentSelector = mkSelector "setCurrent:"

-- | @Selector@ for @previous@
previousSelector :: Selector '[] (Id NSArray)
previousSelector = mkSelector "previous"

-- | @Selector@ for @setPrevious:@
setPreviousSelector :: Selector '[Id NSArray] ()
setPreviousSelector = mkSelector "setPrevious:"

