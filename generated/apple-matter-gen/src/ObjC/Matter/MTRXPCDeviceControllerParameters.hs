{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRXPCDeviceControllerParameters@.
module ObjC.Matter.MTRXPCDeviceControllerParameters
  ( MTRXPCDeviceControllerParameters
  , IsMTRXPCDeviceControllerParameters(..)
  , init_
  , new
  , initWithXPCConnectionBlock_uniqueIdentifier
  , initWithXPConnectionBlock_uniqueIdentifier
  , uniqueIdentifier
  , xpcConnectionBlock
  , initSelector
  , initWithXPCConnectionBlock_uniqueIdentifierSelector
  , initWithXPConnectionBlock_uniqueIdentifierSelector
  , newSelector
  , uniqueIdentifierSelector
  , xpcConnectionBlockSelector


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
init_ :: IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters => mtrxpcDeviceControllerParameters -> IO (Id MTRXPCDeviceControllerParameters)
init_ mtrxpcDeviceControllerParameters =
  sendOwnedMessage mtrxpcDeviceControllerParameters initSelector

-- | @+ new@
new :: IO (Id MTRXPCDeviceControllerParameters)
new  =
  do
    cls' <- getRequiredClass "MTRXPCDeviceControllerParameters"
    sendOwnedClassMessage cls' newSelector

-- | A controller created from this way will connect to a remote instance of an MTRDeviceController loaded in an XPC Service
--
-- @xpcConnectionBlock@ — The XPC Connection block that will return an NSXPCConnection to the intended listener.
--
-- @uniqueIdentifier@ — The unique id to assign to the controller.
--
-- ObjC selector: @- initWithXPCConnectionBlock:uniqueIdentifier:@
initWithXPCConnectionBlock_uniqueIdentifier :: (IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters, IsNSUUID uniqueIdentifier) => mtrxpcDeviceControllerParameters -> Ptr () -> uniqueIdentifier -> IO (Id MTRXPCDeviceControllerParameters)
initWithXPCConnectionBlock_uniqueIdentifier mtrxpcDeviceControllerParameters xpcConnectionBlock uniqueIdentifier =
  sendOwnedMessage mtrxpcDeviceControllerParameters initWithXPCConnectionBlock_uniqueIdentifierSelector xpcConnectionBlock (toNSUUID uniqueIdentifier)

-- | @- initWithXPConnectionBlock:uniqueIdentifier:@
initWithXPConnectionBlock_uniqueIdentifier :: (IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters, IsNSUUID uniqueIdentifier) => mtrxpcDeviceControllerParameters -> Ptr () -> uniqueIdentifier -> IO (Id MTRXPCDeviceControllerParameters)
initWithXPConnectionBlock_uniqueIdentifier mtrxpcDeviceControllerParameters xpcConnectionBlock uniqueIdentifier =
  sendOwnedMessage mtrxpcDeviceControllerParameters initWithXPConnectionBlock_uniqueIdentifierSelector xpcConnectionBlock (toNSUUID uniqueIdentifier)

-- | @- uniqueIdentifier@
uniqueIdentifier :: IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters => mtrxpcDeviceControllerParameters -> IO (Id NSUUID)
uniqueIdentifier mtrxpcDeviceControllerParameters =
  sendMessage mtrxpcDeviceControllerParameters uniqueIdentifierSelector

-- | @- xpcConnectionBlock@
xpcConnectionBlock :: IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters => mtrxpcDeviceControllerParameters -> IO (Ptr ())
xpcConnectionBlock mtrxpcDeviceControllerParameters =
  sendMessage mtrxpcDeviceControllerParameters xpcConnectionBlockSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRXPCDeviceControllerParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRXPCDeviceControllerParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithXPCConnectionBlock:uniqueIdentifier:@
initWithXPCConnectionBlock_uniqueIdentifierSelector :: Selector '[Ptr (), Id NSUUID] (Id MTRXPCDeviceControllerParameters)
initWithXPCConnectionBlock_uniqueIdentifierSelector = mkSelector "initWithXPCConnectionBlock:uniqueIdentifier:"

-- | @Selector@ for @initWithXPConnectionBlock:uniqueIdentifier:@
initWithXPConnectionBlock_uniqueIdentifierSelector :: Selector '[Ptr (), Id NSUUID] (Id MTRXPCDeviceControllerParameters)
initWithXPConnectionBlock_uniqueIdentifierSelector = mkSelector "initWithXPConnectionBlock:uniqueIdentifier:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSUUID)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @xpcConnectionBlock@
xpcConnectionBlockSelector :: Selector '[] (Ptr ())
xpcConnectionBlockSelector = mkSelector "xpcConnectionBlock"

