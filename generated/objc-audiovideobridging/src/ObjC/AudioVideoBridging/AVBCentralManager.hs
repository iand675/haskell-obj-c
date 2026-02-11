{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVBCentralManager
--
-- AVBCentralManager provides centralized management of AVBInterface objects for the network interfaces of the computer.
--
-- AVBCentralManager provides centralized management of the AVBInterface subclasses for the network interfaces of the computer.				Subclasses override the didAddInterface: and didRemoveInterface: methods to be notified when an 				interface is added to or removed from the computer. Addition and removal can happen for any interface but is typically associated				with the Thunderbolt Ethernet Adapter.
--
-- Generated bindings for @AVBCentralManager@.
module ObjC.AudioVideoBridging.AVBCentralManager
  ( AVBCentralManager
  , IsAVBCentralManager(..)
  , startControllerMatching
  , didAddInterface
  , didRemoveInterface
  , streamingEnabledInterfacesOnly
  , nextAvailableDynamicEntityID
  , releaseDynamicEntityID
  , nextAvailableDynamicEntityModelID
  , releaseDynamicEntityModelID
  , startControllerMatchingSelector
  , didAddInterfaceSelector
  , didRemoveInterfaceSelector
  , streamingEnabledInterfacesOnlySelector
  , nextAvailableDynamicEntityIDSelector
  , releaseDynamicEntityIDSelector
  , nextAvailableDynamicEntityModelIDSelector
  , releaseDynamicEntityModelIDSelector


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

-- | This method triggers the IOKit matching for the network controllers.
--
-- This is usually called by a subclass as the last thing in it's init method. This call is broken out of the AVBCentralManager's init method so that 				subclasses can finish their setup before calling it.
--
-- ObjC selector: @- startControllerMatching@
startControllerMatching :: IsAVBCentralManager avbCentralManager => avbCentralManager -> IO ()
startControllerMatching avbCentralManager  =
  sendMsg avbCentralManager (mkSelector "startControllerMatching") retVoid []

-- | This method is called when an AVBInterface object is created for a NIC, either when the central manager is first started up or when the NIC is added later.
--
-- @interface@ — An instance of an AVBInterface subclass (as appropriate for the NIC) which has been added for the discovered NIC.
--
-- The AVBCentralManager maintains it's own internal reference to the interface object until didRemoveInterface: is called with the same interface object,				subclasses do not need to maintain another reference to this. A subclass does not need to call the AVBCentralManager implementation. 				Note this method is not called on the main thread and is not safe for performing UI actions.
--
-- ObjC selector: @- didAddInterface:@
didAddInterface :: (IsAVBCentralManager avbCentralManager, IsAVBInterface interface) => avbCentralManager -> interface -> IO ()
didAddInterface avbCentralManager  interface =
withObjCPtr interface $ \raw_interface ->
    sendMsg avbCentralManager (mkSelector "didAddInterface:") retVoid [argPtr (castPtr raw_interface :: Ptr ())]

-- | This method is called when a NIC has been removed from the system and the central manager is cleaning it up.
--
-- @interface@ — An instance of an AVBInterface subclass (as appropriate for the NIC) which is being removed for the discovered NIC.
--
-- Note this method is not called on the main thread and is not safe for performing UI actions.
--
-- ObjC selector: @- didRemoveInterface:@
didRemoveInterface :: (IsAVBCentralManager avbCentralManager, IsAVBInterface interface) => avbCentralManager -> interface -> IO ()
didRemoveInterface avbCentralManager  interface =
withObjCPtr interface $ \raw_interface ->
    sendMsg avbCentralManager (mkSelector "didRemoveInterface:") retVoid [argPtr (castPtr raw_interface :: Ptr ())]

-- | This method is used to control if the central manager will create and process AVBInterface objects for non streaming interfaces.
--
-- Returns: YES for only AVB Enabled interfaces or NO for all interfaces.
--
-- The default value returned is YES and as such didAddInterface: will be called for all AVB Enabled network interfaces only.
--
-- ObjC selector: @- streamingEnabledInterfacesOnly@
streamingEnabledInterfacesOnly :: IsAVBCentralManager avbCentralManager => avbCentralManager -> IO Bool
streamingEnabledInterfacesOnly avbCentralManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avbCentralManager (mkSelector "streamingEnabledInterfacesOnly") retCULong []

-- | This method is used to allocate a dynamic Entity ID .
--
-- Returns: The allocated dynamic entity ID or AVBNullEUI64 if allocation failed.
--
-- The entity ID allocated by this call can be used for both publishing an Entity with the AVB17221EntityDiscovery class or as a controllerID for the AVB17221ACMPMessage and AVB17221AECPMessage.
--
-- ObjC selector: @+ nextAvailableDynamicEntityID@
nextAvailableDynamicEntityID :: IO CULong
nextAvailableDynamicEntityID  =
  do
    cls' <- getRequiredClass "AVBCentralManager"
    sendClassMsg cls' (mkSelector "nextAvailableDynamicEntityID") retCULong []

-- | This method is used to release a previously allocated dynamic Entity ID.
--
-- @entityID@ — The entity ID to release that was previously allocated by +nextAvailableDynamicEntityID.
--
-- ObjC selector: @+ releaseDynamicEntityID:@
releaseDynamicEntityID :: CULong -> IO ()
releaseDynamicEntityID entityID =
  do
    cls' <- getRequiredClass "AVBCentralManager"
    sendClassMsg cls' (mkSelector "releaseDynamicEntityID:") retVoid [argCULong (fromIntegral entityID)]

-- | This method is used to allocate a dynamic Entity Model ID .
--
-- Returns: The allocated dynamic entity model ID or AVBNullEUI64 if allocation failed.
--
-- The entity model ID allocated by this call can be used for publishing an Entity with the AVB17221EntityDiscovery class when using a dynamically generated or modified model.
--
-- ObjC selector: @+ nextAvailableDynamicEntityModelID@
nextAvailableDynamicEntityModelID :: IO CULong
nextAvailableDynamicEntityModelID  =
  do
    cls' <- getRequiredClass "AVBCentralManager"
    sendClassMsg cls' (mkSelector "nextAvailableDynamicEntityModelID") retCULong []

-- | This method is used to release a previously allocated dynamic Entity Model ID.
--
-- @entityModelID@ — The entity ID to release that was previously allocated by +nextAvailableDynamicEntityModelID.
--
-- ObjC selector: @+ releaseDynamicEntityModelID:@
releaseDynamicEntityModelID :: CULong -> IO ()
releaseDynamicEntityModelID entityModelID =
  do
    cls' <- getRequiredClass "AVBCentralManager"
    sendClassMsg cls' (mkSelector "releaseDynamicEntityModelID:") retVoid [argCULong (fromIntegral entityModelID)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startControllerMatching@
startControllerMatchingSelector :: Selector
startControllerMatchingSelector = mkSelector "startControllerMatching"

-- | @Selector@ for @didAddInterface:@
didAddInterfaceSelector :: Selector
didAddInterfaceSelector = mkSelector "didAddInterface:"

-- | @Selector@ for @didRemoveInterface:@
didRemoveInterfaceSelector :: Selector
didRemoveInterfaceSelector = mkSelector "didRemoveInterface:"

-- | @Selector@ for @streamingEnabledInterfacesOnly@
streamingEnabledInterfacesOnlySelector :: Selector
streamingEnabledInterfacesOnlySelector = mkSelector "streamingEnabledInterfacesOnly"

-- | @Selector@ for @nextAvailableDynamicEntityID@
nextAvailableDynamicEntityIDSelector :: Selector
nextAvailableDynamicEntityIDSelector = mkSelector "nextAvailableDynamicEntityID"

-- | @Selector@ for @releaseDynamicEntityID:@
releaseDynamicEntityIDSelector :: Selector
releaseDynamicEntityIDSelector = mkSelector "releaseDynamicEntityID:"

-- | @Selector@ for @nextAvailableDynamicEntityModelID@
nextAvailableDynamicEntityModelIDSelector :: Selector
nextAvailableDynamicEntityModelIDSelector = mkSelector "nextAvailableDynamicEntityModelID"

-- | @Selector@ for @releaseDynamicEntityModelID:@
releaseDynamicEntityModelIDSelector :: Selector
releaseDynamicEntityModelIDSelector = mkSelector "releaseDynamicEntityModelID:"

