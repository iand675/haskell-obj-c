{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The interface to the Wi-Fi subsystem on OS X.
--
-- Provides access to all Wi-Fi interfaces and allows Wi-Fi clients to setup event notifications.
--
-- CWWiFiClient objects are heavy objects, therefore, clients of the CoreWLAN framework should use a single,  long-running instance rather than creating several short-lived instances.   For convenience, +[CWWiFiClient sharedWiFiClient] can be used to return a singleton instance.
--
-- The CWWiFiClient object should be used to instantiate CWInterface objects rather than using a CWInterface initializer directly.
--
-- Generated bindings for @CWWiFiClient@.
module ObjC.CoreWLAN.CWWiFiClient
  ( CWWiFiClient
  , IsCWWiFiClient(..)
  , sharedWiFiClient
  , init_
  , interface
  , interfaceNames
  , cwWiFiClientInterfaceNames
  , interfaceWithName
  , interfaces
  , startMonitoringEventWithType_error
  , stopMonitoringEventWithType_error
  , stopMonitoringAllEventsAndReturnError
  , delegate
  , setDelegate
  , cwWiFiClientInterfaceNamesSelector
  , delegateSelector
  , initSelector
  , interfaceNamesSelector
  , interfaceSelector
  , interfaceWithNameSelector
  , interfacesSelector
  , setDelegateSelector
  , sharedWiFiClientSelector
  , startMonitoringEventWithType_errorSelector
  , stopMonitoringAllEventsAndReturnErrorSelector
  , stopMonitoringEventWithType_errorSelector

  -- * Enum types
  , CWEventType(CWEventType)
  , pattern CWEventTypeNone
  , pattern CWEventTypePowerDidChange
  , pattern CWEventTypeSSIDDidChange
  , pattern CWEventTypeBSSIDDidChange
  , pattern CWEventTypeCountryCodeDidChange
  , pattern CWEventTypeLinkDidChange
  , pattern CWEventTypeLinkQualityDidChange
  , pattern CWEventTypeModeDidChange
  , pattern CWEventTypeScanCacheUpdated
  , pattern CWEventTypeBtCoexStats
  , pattern CWEventTypeUnknown

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreWLAN.Internal.Classes
import ObjC.CoreWLAN.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns the shared CWWiFiClient instance. There is a single shared instance per process.
--
-- ObjC selector: @+ sharedWiFiClient@
sharedWiFiClient :: IO (Id CWWiFiClient)
sharedWiFiClient  =
  do
    cls' <- getRequiredClass "CWWiFiClient"
    sendClassMessage cls' sharedWiFiClientSelector

-- | Initializes a CWWiFiClient object.
--
-- ObjC selector: @- init@
init_ :: IsCWWiFiClient cwWiFiClient => cwWiFiClient -> IO (Id CWWiFiClient)
init_ cwWiFiClient =
  sendOwnedMessage cwWiFiClient initSelector

-- | Returns the CWInterface object for the default Wi-Fi interface.
--
-- ObjC selector: @- interface@
interface :: IsCWWiFiClient cwWiFiClient => cwWiFiClient -> IO (Id CWInterface)
interface cwWiFiClient =
  sendMessage cwWiFiClient interfaceSelector

-- | Returns: An NSArray of NSString objects corresponding to Wi-Fi interface names.
--
-- Returns the list of available Wi-Fi interface names (e.g. "en0").
--
-- If no Wi-Fi interfaces are available, this method will return an empty array. Returns nil if an error occurs.
--
-- ObjC selector: @- interfaceNames@
interfaceNames :: IsCWWiFiClient cwWiFiClient => cwWiFiClient -> IO (Id NSArray)
interfaceNames cwWiFiClient =
  sendMessage cwWiFiClient interfaceNamesSelector

-- | @+ interfaceNames@
cwWiFiClientInterfaceNames :: IO (Id NSArray)
cwWiFiClientInterfaceNames  =
  do
    cls' <- getRequiredClass "CWWiFiClient"
    sendClassMessage cls' cwWiFiClientInterfaceNamesSelector

-- | @interfaceName@ — The name of an available Wi-Fi interface.
--
-- Get the CWInterface object bound to the Wi-Fi interface with a specific interface name.
--
-- Use +[CWWiFiClient interfaceNames] to get a list of available Wi-Fi interface names. Returns a CWInterface object for the default Wi-Fi interface if no interface name is specified.
--
-- ObjC selector: @- interfaceWithName:@
interfaceWithName :: (IsCWWiFiClient cwWiFiClient, IsNSString interfaceName) => cwWiFiClient -> interfaceName -> IO (Id CWInterface)
interfaceWithName cwWiFiClient interfaceName =
  sendMessage cwWiFiClient interfaceWithNameSelector (toNSString interfaceName)

-- | Returns: An NSArray of CWInterface objects.
--
-- Returns all available Wi-Fi interfaces.
--
-- If no Wi-Fi interfaces are available, this method will return an empty array. Returns nil if an error occurs.
--
-- ObjC selector: @- interfaces@
interfaces :: IsCWWiFiClient cwWiFiClient => cwWiFiClient -> IO (Id NSArray)
interfaces cwWiFiClient =
  sendMessage cwWiFiClient interfacesSelector

-- | @type@ — A CWEventType value.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Register for specific Wi-Fi event notifications.
--
-- ObjC selector: @- startMonitoringEventWithType:error:@
startMonitoringEventWithType_error :: (IsCWWiFiClient cwWiFiClient, IsNSError error_) => cwWiFiClient -> CWEventType -> error_ -> IO Bool
startMonitoringEventWithType_error cwWiFiClient type_ error_ =
  sendMessage cwWiFiClient startMonitoringEventWithType_errorSelector type_ (toNSError error_)

-- | @type@ — A CWEventType value.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Unregister for specific Wi-Fi event notifications.
--
-- ObjC selector: @- stopMonitoringEventWithType:error:@
stopMonitoringEventWithType_error :: (IsCWWiFiClient cwWiFiClient, IsNSError error_) => cwWiFiClient -> CWEventType -> error_ -> IO Bool
stopMonitoringEventWithType_error cwWiFiClient type_ error_ =
  sendMessage cwWiFiClient stopMonitoringEventWithType_errorSelector type_ (toNSError error_)

-- | @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Unregister for all Wi-Fi event notifications.
--
-- ObjC selector: @- stopMonitoringAllEventsAndReturnError:@
stopMonitoringAllEventsAndReturnError :: (IsCWWiFiClient cwWiFiClient, IsNSError error_) => cwWiFiClient -> error_ -> IO Bool
stopMonitoringAllEventsAndReturnError cwWiFiClient error_ =
  sendMessage cwWiFiClient stopMonitoringAllEventsAndReturnErrorSelector (toNSError error_)

-- | Sets the delegate to the specified object, which may implement CWWiFiEventDelegate protocol for Wi-Fi event handling.
--
-- Clients may register for specific Wi-Fi events using -[CWWiFiClient startMonitoringEventWithType:error:].
--
-- ObjC selector: @- delegate@
delegate :: IsCWWiFiClient cwWiFiClient => cwWiFiClient -> IO RawId
delegate cwWiFiClient =
  sendMessage cwWiFiClient delegateSelector

-- | Sets the delegate to the specified object, which may implement CWWiFiEventDelegate protocol for Wi-Fi event handling.
--
-- Clients may register for specific Wi-Fi events using -[CWWiFiClient startMonitoringEventWithType:error:].
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCWWiFiClient cwWiFiClient => cwWiFiClient -> RawId -> IO ()
setDelegate cwWiFiClient value =
  sendMessage cwWiFiClient setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedWiFiClient@
sharedWiFiClientSelector :: Selector '[] (Id CWWiFiClient)
sharedWiFiClientSelector = mkSelector "sharedWiFiClient"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CWWiFiClient)
initSelector = mkSelector "init"

-- | @Selector@ for @interface@
interfaceSelector :: Selector '[] (Id CWInterface)
interfaceSelector = mkSelector "interface"

-- | @Selector@ for @interfaceNames@
interfaceNamesSelector :: Selector '[] (Id NSArray)
interfaceNamesSelector = mkSelector "interfaceNames"

-- | @Selector@ for @interfaceNames@
cwWiFiClientInterfaceNamesSelector :: Selector '[] (Id NSArray)
cwWiFiClientInterfaceNamesSelector = mkSelector "interfaceNames"

-- | @Selector@ for @interfaceWithName:@
interfaceWithNameSelector :: Selector '[Id NSString] (Id CWInterface)
interfaceWithNameSelector = mkSelector "interfaceWithName:"

-- | @Selector@ for @interfaces@
interfacesSelector :: Selector '[] (Id NSArray)
interfacesSelector = mkSelector "interfaces"

-- | @Selector@ for @startMonitoringEventWithType:error:@
startMonitoringEventWithType_errorSelector :: Selector '[CWEventType, Id NSError] Bool
startMonitoringEventWithType_errorSelector = mkSelector "startMonitoringEventWithType:error:"

-- | @Selector@ for @stopMonitoringEventWithType:error:@
stopMonitoringEventWithType_errorSelector :: Selector '[CWEventType, Id NSError] Bool
stopMonitoringEventWithType_errorSelector = mkSelector "stopMonitoringEventWithType:error:"

-- | @Selector@ for @stopMonitoringAllEventsAndReturnError:@
stopMonitoringAllEventsAndReturnErrorSelector :: Selector '[Id NSError] Bool
stopMonitoringAllEventsAndReturnErrorSelector = mkSelector "stopMonitoringAllEventsAndReturnError:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

