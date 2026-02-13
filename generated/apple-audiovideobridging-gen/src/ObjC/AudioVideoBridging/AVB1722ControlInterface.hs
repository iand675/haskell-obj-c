{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB1722ControlInterface
--
-- AVB1722ControlInterface is an abstract class providing the common API for utilizing control services based on IEEE 1722-2011 control frames.
--
-- AVB1722ControlInterface is an abstract class providing the common API for utilizing control services based on IEEE 1722-2011 control frames.				It provides the API for the basic IOKit interactions to talk to the kernel driver.
--
-- Generated bindings for @AVB1722ControlInterface@.
module ObjC.AudioVideoBridging.AVB1722ControlInterface
  ( AVB1722ControlInterface
  , IsAVB1722ControlInterface(..)
  , init_
  , initWithInterfaceName
  , initWithInterface
  , interfaceName
  , interface
  , initSelector
  , initWithInterfaceNameSelector
  , initWithInterfaceSelector
  , interfaceNameSelector
  , interfaceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVB1722ControlInterface avB1722ControlInterface => avB1722ControlInterface -> IO (Id AVB1722ControlInterface)
init_ avB1722ControlInterface =
  sendOwnedMessage avB1722ControlInterface initSelector

-- | initWithInterfaceName:
--
-- Initializes the receiver to work with a 1722 control service on the specified interface. The client must have previously be requested to load on the interface.
--
-- @anInterfaceName@ — The BSD name of the interface on which to create the object.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithInterfaceName:@
initWithInterfaceName :: (IsAVB1722ControlInterface avB1722ControlInterface, IsNSString anInterfaceName) => avB1722ControlInterface -> anInterfaceName -> IO (Id AVB1722ControlInterface)
initWithInterfaceName avB1722ControlInterface anInterfaceName =
  sendOwnedMessage avB1722ControlInterface initWithInterfaceNameSelector (toNSString anInterfaceName)

-- | initWithInterface:
--
-- Initializes the receiver to work with a 1722 control service on the specified interface. The client must have previously be requested to load on the interface.
--
-- @anInterface@ — The AVBInterface object of the interface on which to create the object.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithInterface:@
initWithInterface :: (IsAVB1722ControlInterface avB1722ControlInterface, IsAVBInterface anInterface) => avB1722ControlInterface -> anInterface -> IO (Id AVB1722ControlInterface)
initWithInterface avB1722ControlInterface anInterface =
  sendOwnedMessage avB1722ControlInterface initWithInterfaceSelector (toAVBInterface anInterface)

-- | @- interfaceName@
interfaceName :: IsAVB1722ControlInterface avB1722ControlInterface => avB1722ControlInterface -> IO (Id NSString)
interfaceName avB1722ControlInterface =
  sendMessage avB1722ControlInterface interfaceNameSelector

-- | interface
--
-- The AVBInterface object which owns this object. This may be nil if it was not created by an instance of AVBInterface
--
-- ObjC selector: @- interface@
interface :: IsAVB1722ControlInterface avB1722ControlInterface => avB1722ControlInterface -> IO (Id AVBInterface)
interface avB1722ControlInterface =
  sendMessage avB1722ControlInterface interfaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVB1722ControlInterface)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector '[Id NSString] (Id AVB1722ControlInterface)
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @initWithInterface:@
initWithInterfaceSelector :: Selector '[Id AVBInterface] (Id AVB1722ControlInterface)
initWithInterfaceSelector = mkSelector "initWithInterface:"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector '[] (Id NSString)
interfaceNameSelector = mkSelector "interfaceName"

-- | @Selector@ for @interface@
interfaceSelector :: Selector '[] (Id AVBInterface)
interfaceSelector = mkSelector "interface"

