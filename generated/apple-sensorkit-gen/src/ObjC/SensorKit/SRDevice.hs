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
  , nameSelector
  , modelSelector
  , systemNameSelector
  , systemVersionSelector
  , productTypeSelector


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

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ currentDevice@
currentDevice :: IO (Id SRDevice)
currentDevice  =
  do
    cls' <- getRequiredClass "SRDevice"
    sendClassMsg cls' (mkSelector "currentDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
name srDevice  =
    sendMsg srDevice (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- model@
model :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
model srDevice  =
    sendMsg srDevice (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- systemName@
systemName :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
systemName srDevice  =
    sendMsg srDevice (mkSelector "systemName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- systemVersion@
systemVersion :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
systemVersion srDevice  =
    sendMsg srDevice (mkSelector "systemVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- productType@
productType :: IsSRDevice srDevice => srDevice -> IO (Id NSString)
productType srDevice  =
    sendMsg srDevice (mkSelector "productType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentDevice@
currentDeviceSelector :: Selector
currentDeviceSelector = mkSelector "currentDevice"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @systemName@
systemNameSelector :: Selector
systemNameSelector = mkSelector "systemName"

-- | @Selector@ for @systemVersion@
systemVersionSelector :: Selector
systemVersionSelector = mkSelector "systemVersion"

-- | @Selector@ for @productType@
productTypeSelector :: Selector
productTypeSelector = mkSelector "productType"

