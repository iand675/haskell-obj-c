{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRDevice@.
module ObjC.SensorKit.SRDevice
  ( SRDevice
  , IsSRDevice(..)
  , currentDevice
  , name
  , model
  , systemName
  , systemVersion
  , productType
  , currentDeviceSelector
  , modelSelector
  , nameSelector
  , productTypeSelector
  , systemNameSelector
  , systemVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ currentDevice@
currentDevice :: IO (Id SRDevice)
currentDevice  =
  do
    cls' <- getRequiredClass "SRDevice"
    sendClassMessage cls' currentDeviceSelector

-- | @- name@
name :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
name srDevice =
  sendMessage srDevice nameSelector

-- | @- model@
model :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
model srDevice =
  sendMessage srDevice modelSelector

-- | @- systemName@
systemName :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
systemName srDevice =
  sendMessage srDevice systemNameSelector

-- | @- systemVersion@
systemVersion :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
systemVersion srDevice =
  sendMessage srDevice systemVersionSelector

-- | @- productType@
productType :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
productType srDevice =
  sendMessage srDevice productTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentDevice@
currentDeviceSelector :: Selector '[] (Id SRDevice)
currentDeviceSelector = mkSelector "currentDevice"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id NSString)
modelSelector = mkSelector "model"

-- | @Selector@ for @systemName@
systemNameSelector :: Selector '[] (Id NSString)
systemNameSelector = mkSelector "systemName"

-- | @Selector@ for @systemVersion@
systemVersionSelector :: Selector '[] (Id NSString)
systemVersionSelector = mkSelector "systemVersion"

-- | @Selector@ for @productType@
productTypeSelector :: Selector '[] (Id NSString)
productTypeSelector = mkSelector "productType"

