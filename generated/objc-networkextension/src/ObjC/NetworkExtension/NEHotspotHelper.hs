{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotHelper
--
-- The NEHotspotHelper class allows an application to register itself as a   HotspotHelper.
--
-- Generated bindings for @NEHotspotHelper@.
module ObjC.NetworkExtension.NEHotspotHelper
  ( NEHotspotHelper
  , IsNEHotspotHelper(..)
  , registerWithOptions_queue_handler
  , logoff
  , supportedNetworkInterfaces
  , registerWithOptions_queue_handlerSelector
  , logoffSelector
  , supportedNetworkInterfacesSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | registerWithOptions:queue:handler
--
-- Register the application as a HotspotHelper.
--
-- Once this API is invoked successfully, the application becomes   eligible to be launched in the background and participate in   various hotspot related functions.
--
-- This function should be called once when the application starts up.   Invoking it again will have no effect and result in FALSE being returned.
--
-- The 'options' dictionary may be nil, or contain the single property   kNEHotspotHelperOptionDisplayName.
--
-- @options@ — If not nil, 'options' is an NSDictionary containing   kNEHotspotHelperOption* keys (currently just   kNEHotspotHelperOptionDisplayName).
--
-- @queue@ — The dispatch_queue_t to invoke the handle block on.
--
-- @handler@ — The NEHotspotHelperHandler block to execute to process   helper commands.
--
-- Returns: YES if the registration was successful, NO otherwise.
--
-- Note: Notes
--
-- Note: 1   The application's Info.plist MUST include a UIBackgroundModes array   containing 'network-authentication'.
--
-- Note: 2   The application MUST set 'com.apple.developer.networking.HotspotHelper'   as one of its entitlements. The value of the entitlement is a boolean   value true.
--
-- ObjC selector: @+ registerWithOptions:queue:handler:@
registerWithOptions_queue_handler :: (IsNSDictionary options, IsNSObject queue) => options -> queue -> Ptr () -> IO Bool
registerWithOptions_queue_handler options queue handler =
  do
    cls' <- getRequiredClass "NEHotspotHelper"
    withObjCPtr options $ \raw_options ->
      withObjCPtr queue $ \raw_queue ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "registerWithOptions:queue:handler:") retCULong [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | logoff:
--
-- Terminate the authentication session.
--
-- The application invokes this method when it wants to logoff from the   current network. Invoking this method causes an NEHotspotHelperCommand   of type kNEHotspotHelperCommandTypeLogoff to be issued to the application's   'handler' block (see +[NEHotspotHelper registerWithOptions:queue:handler]).
--
-- 'network' must correspond to the currently associated Wi-Fi network   i.e. it must have come from the NEHotspotHelperCommand's 'network' property   or from the +[NEHotspotHelper supportedInterfaces] method.
--
-- Returns: YES if the logoff command was successfully queued, NO otherwise.
--
-- Note: Notes
--
-- Note: 1   The application MUST NOT actually logoff from the network until it   receives the command to logoff.
--
-- Note: 2   After the application invokes -[NEHotspotHelperResponse deliver] indicating   kNEHotspotHelperResultSuccess, the Wi-Fi network is disassociated.
--
-- ObjC selector: @+ logoff:@
logoff :: IsNEHotspotNetwork network => network -> IO Bool
logoff network =
  do
    cls' <- getRequiredClass "NEHotspotHelper"
    withObjCPtr network $ \raw_network ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "logoff:") retCULong [argPtr (castPtr raw_network :: Ptr ())]

-- | supportedNetworkInterfaces
--
-- Return the list of network interfaces managed by the   HotspotHelper infrastructure.
--
-- Each network interface is represented by an NEHotspotNetwork object.   Currently, the returned array contains exactly one NEHotspotNetwork   object representing the Wi-Fi interface.
--
-- The main purpose of this method is to allow a HotspotHelper to provide   accurate status in its UI at times when it has not been given a command   to process. This method coupled with -[NEHotspotNetwork isChosenHelper]   allows the application to know whether it is the one that is handling   the current network.
--
-- Returns: nil if no network interfaces are being managed,   non-nil NSArray of NEHotspotNetwork objects otherwise.
--
-- ObjC selector: @+ supportedNetworkInterfaces@
supportedNetworkInterfaces :: IO (Id NSArray)
supportedNetworkInterfaces  =
  do
    cls' <- getRequiredClass "NEHotspotHelper"
    sendClassMsg cls' (mkSelector "supportedNetworkInterfaces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerWithOptions:queue:handler:@
registerWithOptions_queue_handlerSelector :: Selector
registerWithOptions_queue_handlerSelector = mkSelector "registerWithOptions:queue:handler:"

-- | @Selector@ for @logoff:@
logoffSelector :: Selector
logoffSelector = mkSelector "logoff:"

-- | @Selector@ for @supportedNetworkInterfaces@
supportedNetworkInterfacesSelector :: Selector
supportedNetworkInterfacesSelector = mkSelector "supportedNetworkInterfaces"

