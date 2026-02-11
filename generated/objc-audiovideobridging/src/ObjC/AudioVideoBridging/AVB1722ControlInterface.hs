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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVB1722ControlInterface avB1722ControlInterface => avB1722ControlInterface -> IO (Id AVB1722ControlInterface)
init_ avB1722ControlInterface  =
  sendMsg avB1722ControlInterface (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithInterfaceName avB1722ControlInterface  anInterfaceName =
withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
    sendMsg avB1722ControlInterface (mkSelector "initWithInterfaceName:") (retPtr retVoid) [argPtr (castPtr raw_anInterfaceName :: Ptr ())] >>= ownedObject . castPtr

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
initWithInterface avB1722ControlInterface  anInterface =
withObjCPtr anInterface $ \raw_anInterface ->
    sendMsg avB1722ControlInterface (mkSelector "initWithInterface:") (retPtr retVoid) [argPtr (castPtr raw_anInterface :: Ptr ())] >>= ownedObject . castPtr

-- | @- interfaceName@
interfaceName :: IsAVB1722ControlInterface avB1722ControlInterface => avB1722ControlInterface -> IO (Id NSString)
interfaceName avB1722ControlInterface  =
  sendMsg avB1722ControlInterface (mkSelector "interfaceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | interface
--
-- The AVBInterface object which owns this object. This may be nil if it was not created by an instance of AVBInterface
--
-- ObjC selector: @- interface@
interface :: IsAVB1722ControlInterface avB1722ControlInterface => avB1722ControlInterface -> IO (Id AVBInterface)
interface avB1722ControlInterface  =
  sendMsg avB1722ControlInterface (mkSelector "interface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @initWithInterface:@
initWithInterfaceSelector :: Selector
initWithInterfaceSelector = mkSelector "initWithInterface:"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector
interfaceNameSelector = mkSelector "interfaceName"

-- | @Selector@ for @interface@
interfaceSelector :: Selector
interfaceSelector = mkSelector "interface"

