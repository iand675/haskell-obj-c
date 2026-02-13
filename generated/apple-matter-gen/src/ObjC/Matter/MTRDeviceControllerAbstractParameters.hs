{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Parameters that can be used to initialize an MTRDeviceController.  Specific interfaces inheriting from this one should be used to actually do the initialization.
--
-- Generated bindings for @MTRDeviceControllerAbstractParameters@.
module ObjC.Matter.MTRDeviceControllerAbstractParameters
  ( MTRDeviceControllerAbstractParameters
  , IsMTRDeviceControllerAbstractParameters(..)
  , init_
  , new
  , startSuspended
  , setStartSuspended
  , initSelector
  , newSelector
  , setStartSuspendedSelector
  , startSuspendedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceControllerAbstractParameters mtrDeviceControllerAbstractParameters => mtrDeviceControllerAbstractParameters -> IO (Id MTRDeviceControllerAbstractParameters)
init_ mtrDeviceControllerAbstractParameters =
  sendOwnedMessage mtrDeviceControllerAbstractParameters initSelector

-- | @+ new@
new :: IO (Id MTRDeviceControllerAbstractParameters)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerAbstractParameters"
    sendOwnedClassMessage cls' newSelector

-- | Whether the controller should start out suspended.
--
-- Defaults to NO.
--
-- ObjC selector: @- startSuspended@
startSuspended :: IsMTRDeviceControllerAbstractParameters mtrDeviceControllerAbstractParameters => mtrDeviceControllerAbstractParameters -> IO Bool
startSuspended mtrDeviceControllerAbstractParameters =
  sendMessage mtrDeviceControllerAbstractParameters startSuspendedSelector

-- | Whether the controller should start out suspended.
--
-- Defaults to NO.
--
-- ObjC selector: @- setStartSuspended:@
setStartSuspended :: IsMTRDeviceControllerAbstractParameters mtrDeviceControllerAbstractParameters => mtrDeviceControllerAbstractParameters -> Bool -> IO ()
setStartSuspended mtrDeviceControllerAbstractParameters value =
  sendMessage mtrDeviceControllerAbstractParameters setStartSuspendedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceControllerAbstractParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceControllerAbstractParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @startSuspended@
startSuspendedSelector :: Selector '[] Bool
startSuspendedSelector = mkSelector "startSuspended"

-- | @Selector@ for @setStartSuspended:@
setStartSuspendedSelector :: Selector '[Bool] ()
setStartSuspendedSelector = mkSelector "setStartSuspended:"

