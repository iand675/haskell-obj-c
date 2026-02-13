{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideOfferResponseParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideOfferResponseParams
  ( MTRWebRTCTransportProviderClusterProvideOfferResponseParams
  , IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams(..)
  , initWithResponseValue_error
  , webRTCSessionID
  , setWebRTCSessionID
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , audioStreamIDSelector
  , initWithResponseValue_errorSelector
  , setAudioStreamIDSelector
  , setVideoStreamIDSelector
  , setWebRTCSessionIDSelector
  , videoStreamIDSelector
  , webRTCSessionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRWebRTCTransportProviderClusterProvideOfferResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> responseValue -> error_ -> IO (Id MTRWebRTCTransportProviderClusterProvideOfferResponseParams)
initWithResponseValue_error mtrWebRTCTransportProviderClusterProvideOfferResponseParams responseValue error_ =
  sendOwnedMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams setVideoStreamIDSelector (toNSNumber value)

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferResponseParams setAudioStreamIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRWebRTCTransportProviderClusterProvideOfferResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector '[] (Id NSNumber)
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector '[Id NSNumber] ()
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector '[] (Id NSNumber)
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector '[Id NSNumber] ()
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

