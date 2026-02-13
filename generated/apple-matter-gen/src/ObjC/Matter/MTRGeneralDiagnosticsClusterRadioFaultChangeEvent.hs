{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterRadioFaultChangeEvent@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterRadioFaultChangeEvent
  ( MTRGeneralDiagnosticsClusterRadioFaultChangeEvent
  , IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent(..)
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
current :: IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> IO (Id NSArray)
current mtrGeneralDiagnosticsClusterRadioFaultChangeEvent =
  sendMessage mtrGeneralDiagnosticsClusterRadioFaultChangeEvent currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> value -> IO ()
setCurrent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent value =
  sendMessage mtrGeneralDiagnosticsClusterRadioFaultChangeEvent setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> IO (Id NSArray)
previous mtrGeneralDiagnosticsClusterRadioFaultChangeEvent =
  sendMessage mtrGeneralDiagnosticsClusterRadioFaultChangeEvent previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> value -> IO ()
setPrevious mtrGeneralDiagnosticsClusterRadioFaultChangeEvent value =
  sendMessage mtrGeneralDiagnosticsClusterRadioFaultChangeEvent setPreviousSelector (toNSArray value)

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

