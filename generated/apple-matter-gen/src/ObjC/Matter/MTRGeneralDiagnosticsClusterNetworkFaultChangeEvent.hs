{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterNetworkFaultChangeEvent@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterNetworkFaultChangeEvent
  ( MTRGeneralDiagnosticsClusterNetworkFaultChangeEvent
  , IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent(..)
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
current :: IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> IO (Id NSArray)
current mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent =
  sendMessage mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> value -> IO ()
setCurrent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> IO (Id NSArray)
previous mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent =
  sendMessage mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> value -> IO ()
setPrevious mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent setPreviousSelector (toNSArray value)

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

