{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLaunchURLParams@.
module ObjC.Matter.MTRContentLauncherClusterLaunchURLParams
  ( MTRContentLauncherClusterLaunchURLParams
  , IsMTRContentLauncherClusterLaunchURLParams(..)
  , contentURL
  , setContentURL
  , displayString
  , setDisplayString
  , brandingInformation
  , setBrandingInformation
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , brandingInformationSelector
  , contentURLSelector
  , displayStringSelector
  , serverSideProcessingTimeoutSelector
  , setBrandingInformationSelector
  , setContentURLSelector
  , setDisplayStringSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contentURL@
contentURL :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSString)
contentURL mtrContentLauncherClusterLaunchURLParams =
  sendMessage mtrContentLauncherClusterLaunchURLParams contentURLSelector

-- | @- setContentURL:@
setContentURL :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSString value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setContentURL mtrContentLauncherClusterLaunchURLParams value =
  sendMessage mtrContentLauncherClusterLaunchURLParams setContentURLSelector (toNSString value)

-- | @- displayString@
displayString :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSString)
displayString mtrContentLauncherClusterLaunchURLParams =
  sendMessage mtrContentLauncherClusterLaunchURLParams displayStringSelector

-- | @- setDisplayString:@
setDisplayString :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSString value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setDisplayString mtrContentLauncherClusterLaunchURLParams value =
  sendMessage mtrContentLauncherClusterLaunchURLParams setDisplayStringSelector (toNSString value)

-- | @- brandingInformation@
brandingInformation :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id MTRContentLauncherClusterBrandingInformationStruct)
brandingInformation mtrContentLauncherClusterLaunchURLParams =
  sendMessage mtrContentLauncherClusterLaunchURLParams brandingInformationSelector

-- | @- setBrandingInformation:@
setBrandingInformation :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsMTRContentLauncherClusterBrandingInformationStruct value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setBrandingInformation mtrContentLauncherClusterLaunchURLParams value =
  sendMessage mtrContentLauncherClusterLaunchURLParams setBrandingInformationSelector (toMTRContentLauncherClusterBrandingInformationStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLaunchURLParams =
  sendMessage mtrContentLauncherClusterLaunchURLParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSNumber value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLaunchURLParams value =
  sendMessage mtrContentLauncherClusterLaunchURLParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams => mtrContentLauncherClusterLaunchURLParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentLauncherClusterLaunchURLParams =
  sendMessage mtrContentLauncherClusterLaunchURLParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentLauncherClusterLaunchURLParams mtrContentLauncherClusterLaunchURLParams, IsNSNumber value) => mtrContentLauncherClusterLaunchURLParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentLauncherClusterLaunchURLParams value =
  sendMessage mtrContentLauncherClusterLaunchURLParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentURL@
contentURLSelector :: Selector '[] (Id NSString)
contentURLSelector = mkSelector "contentURL"

-- | @Selector@ for @setContentURL:@
setContentURLSelector :: Selector '[Id NSString] ()
setContentURLSelector = mkSelector "setContentURL:"

-- | @Selector@ for @displayString@
displayStringSelector :: Selector '[] (Id NSString)
displayStringSelector = mkSelector "displayString"

-- | @Selector@ for @setDisplayString:@
setDisplayStringSelector :: Selector '[Id NSString] ()
setDisplayStringSelector = mkSelector "setDisplayString:"

-- | @Selector@ for @brandingInformation@
brandingInformationSelector :: Selector '[] (Id MTRContentLauncherClusterBrandingInformationStruct)
brandingInformationSelector = mkSelector "brandingInformation"

-- | @Selector@ for @setBrandingInformation:@
setBrandingInformationSelector :: Selector '[Id MTRContentLauncherClusterBrandingInformationStruct] ()
setBrandingInformationSelector = mkSelector "setBrandingInformation:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

