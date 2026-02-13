{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , unconfiguredAccessories
  , configureAccessory_withConfigurationUIOnViewControllerSelector
  , delegateSelector
  , initWithDelegate_queueSelector
  , setDelegateSelector
  , startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector
  , stopSearchingForUnconfiguredAccessoriesSelector
  , unconfiguredAccessoriesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDelegate_queue eaWiFiUnconfiguredAccessoryBrowser delegate queue =
  sendOwnedMessage eaWiFiUnconfiguredAccessoryBrowser initWithDelegate_queueSelector delegate (toNSObject queue)

-- | Start the search for unconfigured accessories
--
-- Starts a Wi-Fi scan for unconfigured accessories. This power and resource intensive process and must             only be used when actively searching for accessories. Scans should be stopped immediately when the             desired accessories have been located.
--
-- @predicate@ — The desired filter for unconfigured accessory results conforming to the EAWiFiUnconfiguredAccessory protocol.
--
-- ObjC selector: @- startSearchingForUnconfiguredAccessoriesMatchingPredicate:@
startSearchingForUnconfiguredAccessoriesMatchingPredicate :: (IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser, IsNSPredicate predicate) => eaWiFiUnconfiguredAccessoryBrowser -> predicate -> IO ()
startSearchingForUnconfiguredAccessoriesMatchingPredicate eaWiFiUnconfiguredAccessoryBrowser predicate =
  sendMessage eaWiFiUnconfiguredAccessoryBrowser startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector (toNSPredicate predicate)

-- | Stop the search for unconfigured MFi Wireless Accessory Configuration accessories
--
-- ObjC selector: @- stopSearchingForUnconfiguredAccessories@
stopSearchingForUnconfiguredAccessories :: IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser => eaWiFiUnconfiguredAccessoryBrowser -> IO ()
stopSearchingForUnconfiguredAccessories eaWiFiUnconfiguredAccessoryBrowser =
  sendMessage eaWiFiUnconfiguredAccessoryBrowser stopSearchingForUnconfiguredAccessoriesSelector

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
configureAccessory_withConfigurationUIOnViewController eaWiFiUnconfiguredAccessoryBrowser accessory viewController =
  sendMessage eaWiFiUnconfiguredAccessoryBrowser configureAccessory_withConfigurationUIOnViewControllerSelector (toEAWiFiUnconfiguredAccessory accessory) (toUIViewController viewController)

-- | delegate
--
-- The delegate object that will receive the browser events.
--
-- ObjC selector: @- delegate@
delegate :: IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser => eaWiFiUnconfiguredAccessoryBrowser -> IO RawId
delegate eaWiFiUnconfiguredAccessoryBrowser =
  sendMessage eaWiFiUnconfiguredAccessoryBrowser delegateSelector

-- | delegate
--
-- The delegate object that will receive the browser events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser => eaWiFiUnconfiguredAccessoryBrowser -> RawId -> IO ()
setDelegate eaWiFiUnconfiguredAccessoryBrowser value =
  sendMessage eaWiFiUnconfiguredAccessoryBrowser setDelegateSelector value

-- | unconfiguredAccessories
--
-- The set of discovered unconfigured accessories described by EAWiFiUnconfiguredAccessory objects.             This snapshot will only include objects matching the filter predicate defined when starting the search.
--
-- ObjC selector: @- unconfiguredAccessories@
unconfiguredAccessories :: IsEAWiFiUnconfiguredAccessoryBrowser eaWiFiUnconfiguredAccessoryBrowser => eaWiFiUnconfiguredAccessoryBrowser -> IO (Id NSSet)
unconfiguredAccessories eaWiFiUnconfiguredAccessoryBrowser =
  sendMessage eaWiFiUnconfiguredAccessoryBrowser unconfiguredAccessoriesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDelegate:queue:@
initWithDelegate_queueSelector :: Selector '[RawId, Id NSObject] (Id EAWiFiUnconfiguredAccessoryBrowser)
initWithDelegate_queueSelector = mkSelector "initWithDelegate:queue:"

-- | @Selector@ for @startSearchingForUnconfiguredAccessoriesMatchingPredicate:@
startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector :: Selector '[Id NSPredicate] ()
startSearchingForUnconfiguredAccessoriesMatchingPredicateSelector = mkSelector "startSearchingForUnconfiguredAccessoriesMatchingPredicate:"

-- | @Selector@ for @stopSearchingForUnconfiguredAccessories@
stopSearchingForUnconfiguredAccessoriesSelector :: Selector '[] ()
stopSearchingForUnconfiguredAccessoriesSelector = mkSelector "stopSearchingForUnconfiguredAccessories"

-- | @Selector@ for @configureAccessory:withConfigurationUIOnViewController:@
configureAccessory_withConfigurationUIOnViewControllerSelector :: Selector '[Id EAWiFiUnconfiguredAccessory, Id UIViewController] ()
configureAccessory_withConfigurationUIOnViewControllerSelector = mkSelector "configureAccessory:withConfigurationUIOnViewController:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @unconfiguredAccessories@
unconfiguredAccessoriesSelector :: Selector '[] (Id NSSet)
unconfiguredAccessoriesSelector = mkSelector "unconfiguredAccessories"

