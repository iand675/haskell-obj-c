{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Interface for browsing unconfigured accessories
--
-- This class brokers access to the MFi Wireless Accessory Configuration (WAC) process.             This browser enables the application to scan for unconfigured accessories,             connect them to the user's Wi-Fi infrastructure and configure attributes of             the accessory.
--
-- Generated bindings for @EAWiFiUnconfiguredAccessoryBrowser@.
module ObjC.ExternalAccessory.EAWiFiUnconfiguredAccessoryBrowser
  ( EAWiFiUnconfiguredAccessoryBrowser
  , IsEAWiFiUnconfiguredAccessoryBrowser(..)
  , initWithDelegate_queue
  , startSearchingForUnconfiguredAccessoriesMatchingPredicate
  , stopSearchingForUnconfiguredAccessories
  , configureAccessory_withConfigurationUIOnViewController
  , unconfiguredAccessories
  , initWithDelegate_queueSelector
  , startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector
  , stopSearchingForUnconfiguredAccessoriesSelector
  , configureAccessory_withConfigurationUIOnViewControllerSelector
  , unconfiguredAccessoriesSelector


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

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Designated initializer.
--
-- Initializes an instance of the EAWiFiUnconfiguredAccessoryBrowser class             which can be further configured based on the application's interests.
--
-- @delegate@ — The delegate that will receive the EAWiFiUnconfiguredAccessoryBrowserDelegate events.
--
-- @queue@ — The dispatch queue the delegate would like to receive events on. If nil the events will be on the main queue.
--
-- Returns: Instance object
--
-- ObjC selector: @- initWithDelegate:queue:@
initWithDelegate_queue :: (IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser, IsNSObject queue) => eaWiFiUnconfiguredAccessoryBrowser -> RawId -> queue -> IO (Id EAWiFiUnconfiguredAccessoryBrowser)
initWithDelegate_queue eaWiFiUnconfiguredAccessoryBrowser  delegate queue =
withObjCPtr queue $ \raw_queue ->
    sendMsg eaWiFiUnconfiguredAccessoryBrowser (mkSelector "initWithDelegate:queue:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | Start the search for unconfigured accessories
--
-- Starts a Wi-Fi scan for unconfigured accessories. This power and resource intensive process and must             only be used when actively searching for accessories. Scans should be stopped immediately when the             desired accessories have been located.
--
-- @predicate@ — The desired filter for unconfigured accessory results conforming to the EAWiFiUnconfiguredAccessory protocol.
--
-- ObjC selector: @- startSearchingForUnconfiguredAccessoriesMatchingPredicate:@
startSearchingForUnconfiguredAccessoriesMatchingPredicate :: (IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser, IsNSPredicate predicate) => eaWiFiUnconfiguredAccessoryBrowser -> predicate -> IO ()
startSearchingForUnconfiguredAccessoriesMatchingPredicate eaWiFiUnconfiguredAccessoryBrowser  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg eaWiFiUnconfiguredAccessoryBrowser (mkSelector "startSearchingForUnconfiguredAccessoriesMatchingPredicate:") retVoid [argPtr (castPtr raw_predicate :: Ptr ())]

-- | Stop the search for unconfigured MFi Wireless Accessory Configuration accessories
--
-- ObjC selector: @- stopSearchingForUnconfiguredAccessories@
stopSearchingForUnconfiguredAccessories :: IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser => eaWiFiUnconfiguredAccessoryBrowser -> IO ()
stopSearchingForUnconfiguredAccessories eaWiFiUnconfiguredAccessoryBrowser  =
  sendMsg eaWiFiUnconfiguredAccessoryBrowser (mkSelector "stopSearchingForUnconfiguredAccessories") retVoid []

-- | Begin the configuration process for the chosen accessory
--
-- Stop the search for unconfigured accessories and begins the configuration process of the specified EAWiFiUnconfiguredAccessory.             The user is guided through the configuration process via Apple UI. This process can take up to a few minutes to complete.             The host application delegate will receive the didFinishConfiguringAccessory callback with an error that should be             checked upon completion.
--
-- @accessory@ — The accessory the application wishes to configure
--
-- @viewController@ — The UIViewController that will host the Apple guided setup UI in the host application.
--
-- ObjC selector: @- configureAccessory:withConfigurationUIOnViewController:@
configureAccessory_withConfigurationUIOnViewController :: (IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser, IsEAWiFiUnconfiguredAccessory accessory, IsUIViewController viewController) => eaWiFiUnconfiguredAccessoryBrowser -> accessory -> viewController -> IO ()
configureAccessory_withConfigurationUIOnViewController eaWiFiUnconfiguredAccessoryBrowser  accessory viewController =
withObjCPtr accessory $ \raw_accessory ->
  withObjCPtr viewController $ \raw_viewController ->
      sendMsg eaWiFiUnconfiguredAccessoryBrowser (mkSelector "configureAccessory:withConfigurationUIOnViewController:") retVoid [argPtr (castPtr raw_accessory :: Ptr ()), argPtr (castPtr raw_viewController :: Ptr ())]

-- | unconfiguredAccessories
--
-- The set of discovered unconfigured accessories described by EAWiFiUnconfiguredAccessory objects.             This snapshot will only include objects matching the filter predicate defined when starting the search.
--
-- ObjC selector: @- unconfiguredAccessories@
unconfiguredAccessories :: IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser => eaWiFiUnconfiguredAccessoryBrowser -> IO (Id NSSet)
unconfiguredAccessories eaWiFiUnconfiguredAccessoryBrowser  =
  sendMsg eaWiFiUnconfiguredAccessoryBrowser (mkSelector "unconfiguredAccessories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDelegate:queue:@
initWithDelegate_queueSelector :: Selector
initWithDelegate_queueSelector = mkSelector "initWithDelegate:queue:"

-- | @Selector@ for @startSearchingForUnconfiguredAccessoriesMatchingPredicate:@
startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector :: Selector
startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector = mkSelector "startSearchingForUnconfiguredAccessoriesMatchingPredicate:"

-- | @Selector@ for @stopSearchingForUnconfiguredAccessories@
stopSearchingForUnconfiguredAccessoriesSelector :: Selector
stopSearchingForUnconfiguredAccessoriesSelector = mkSelector "stopSearchingForUnconfiguredAccessories"

-- | @Selector@ for @configureAccessory:withConfigurationUIOnViewController:@
configureAccessory_withConfigurationUIOnViewControllerSelector :: Selector
configureAccessory_withConfigurationUIOnViewControllerSelector = mkSelector "configureAccessory:withConfigurationUIOnViewController:"

-- | @Selector@ for @unconfiguredAccessories@
unconfiguredAccessoriesSelector :: Selector
unconfiguredAccessoriesSelector = mkSelector "unconfiguredAccessories"

