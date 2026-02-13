{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateProviderClusterQueryImageResponseParams@.
module ObjC.Matter.MTROTASoftwareUpdateProviderClusterQueryImageResponseParams
  ( MTROTASoftwareUpdateProviderClusterQueryImageResponseParams
  , IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , delayedActionTime
  , setDelayedActionTime
  , imageURI
  , setImageURI
  , softwareVersion
  , setSoftwareVersion
  , softwareVersionString
  , setSoftwareVersionString
  , updateToken
  , setUpdateToken
  , userConsentNeeded
  , setUserConsentNeeded
  , metadataForRequestor
  , setMetadataForRequestor
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , delayedActionTimeSelector
  , imageURISelector
  , initWithResponseValue_errorSelector
  , metadataForRequestorSelector
  , setDelayedActionTimeSelector
  , setImageURISelector
  , setMetadataForRequestorSelector
  , setSoftwareVersionSelector
  , setSoftwareVersionStringSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , setUpdateTokenSelector
  , setUserConsentNeededSelector
  , softwareVersionSelector
  , softwareVersionStringSelector
  , statusSelector
  , timedInvokeTimeoutMsSelector
  , updateTokenSelector
  , userConsentNeededSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTROTASoftwareUpdateProviderClusterQueryImageResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> responseValue -> error_ -> IO (Id MTROTASoftwareUpdateProviderClusterQueryImageResponseParams)
initWithResponseValue_error mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams responseValue error_ =
  sendOwnedMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
status mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setStatus mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setStatusSelector (toNSNumber value)

-- | @- delayedActionTime@
delayedActionTime :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
delayedActionTime mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams delayedActionTimeSelector

-- | @- setDelayedActionTime:@
setDelayedActionTime :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setDelayedActionTime mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setDelayedActionTimeSelector (toNSNumber value)

-- | @- imageURI@
imageURI :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSString)
imageURI mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams imageURISelector

-- | @- setImageURI:@
setImageURI :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSString value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setImageURI mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setImageURISelector (toNSString value)

-- | @- softwareVersion@
softwareVersion :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
softwareVersion mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setSoftwareVersion mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setSoftwareVersionSelector (toNSNumber value)

-- | @- softwareVersionString@
softwareVersionString :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSString)
softwareVersionString mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams softwareVersionStringSelector

-- | @- setSoftwareVersionString:@
setSoftwareVersionString :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSString value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setSoftwareVersionString mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setSoftwareVersionStringSelector (toNSString value)

-- | @- updateToken@
updateToken :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSData)
updateToken mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams updateTokenSelector

-- | @- setUpdateToken:@
setUpdateToken :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSData value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setUpdateToken mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setUpdateTokenSelector (toNSData value)

-- | @- userConsentNeeded@
userConsentNeeded :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
userConsentNeeded mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams userConsentNeededSelector

-- | @- setUserConsentNeeded:@
setUserConsentNeeded :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setUserConsentNeeded mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setUserConsentNeededSelector (toNSNumber value)

-- | @- metadataForRequestor@
metadataForRequestor :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSData)
metadataForRequestor mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams metadataForRequestorSelector

-- | @- setMetadataForRequestor:@
setMetadataForRequestor :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSData value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setMetadataForRequestor mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setMetadataForRequestorSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROTASoftwareUpdateProviderClusterQueryImageResponseParams mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterQueryImageResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROTASoftwareUpdateProviderClusterQueryImageResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @delayedActionTime@
delayedActionTimeSelector :: Selector '[] (Id NSNumber)
delayedActionTimeSelector = mkSelector "delayedActionTime"

-- | @Selector@ for @setDelayedActionTime:@
setDelayedActionTimeSelector :: Selector '[Id NSNumber] ()
setDelayedActionTimeSelector = mkSelector "setDelayedActionTime:"

-- | @Selector@ for @imageURI@
imageURISelector :: Selector '[] (Id NSString)
imageURISelector = mkSelector "imageURI"

-- | @Selector@ for @setImageURI:@
setImageURISelector :: Selector '[Id NSString] ()
setImageURISelector = mkSelector "setImageURI:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @softwareVersionString@
softwareVersionStringSelector :: Selector '[] (Id NSString)
softwareVersionStringSelector = mkSelector "softwareVersionString"

-- | @Selector@ for @setSoftwareVersionString:@
setSoftwareVersionStringSelector :: Selector '[Id NSString] ()
setSoftwareVersionStringSelector = mkSelector "setSoftwareVersionString:"

-- | @Selector@ for @updateToken@
updateTokenSelector :: Selector '[] (Id NSData)
updateTokenSelector = mkSelector "updateToken"

-- | @Selector@ for @setUpdateToken:@
setUpdateTokenSelector :: Selector '[Id NSData] ()
setUpdateTokenSelector = mkSelector "setUpdateToken:"

-- | @Selector@ for @userConsentNeeded@
userConsentNeededSelector :: Selector '[] (Id NSNumber)
userConsentNeededSelector = mkSelector "userConsentNeeded"

-- | @Selector@ for @setUserConsentNeeded:@
setUserConsentNeededSelector :: Selector '[Id NSNumber] ()
setUserConsentNeededSelector = mkSelector "setUserConsentNeeded:"

-- | @Selector@ for @metadataForRequestor@
metadataForRequestorSelector :: Selector '[] (Id NSData)
metadataForRequestorSelector = mkSelector "metadataForRequestor"

-- | @Selector@ for @setMetadataForRequestor:@
setMetadataForRequestorSelector :: Selector '[Id NSData] ()
setMetadataForRequestorSelector = mkSelector "setMetadataForRequestor:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

