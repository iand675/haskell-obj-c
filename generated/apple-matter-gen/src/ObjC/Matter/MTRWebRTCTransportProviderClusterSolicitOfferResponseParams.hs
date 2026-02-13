{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterSolicitOfferResponseParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterSolicitOfferResponseParams
  ( MTRWebRTCTransportProviderClusterSolicitOfferResponseParams
  , IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams(..)
  , initWithResponseValue_error
  , webRTCSessionID
  , setWebRTCSessionID
  , deferredOffer
  , setDeferredOffer
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , audioStreamIDSelector
  , deferredOfferSelector
  , initWithResponseValue_errorSelector
  , setAudioStreamIDSelector
  , setDeferredOfferSelector
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

-- | Initialize an MTRWebRTCTransportProviderClusterSolicitOfferResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> responseValue -> error_ -> IO (Id MTRWebRTCTransportProviderClusterSolicitOfferResponseParams)
initWithResponseValue_error mtrWebRTCTransportProviderClusterSolicitOfferResponseParams responseValue error_ =
  sendOwnedMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- deferredOffer@
deferredOffer :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
deferredOffer mtrWebRTCTransportProviderClusterSolicitOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams deferredOfferSelector

-- | @- setDeferredOffer:@
setDeferredOffer :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setDeferredOffer mtrWebRTCTransportProviderClusterSolicitOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams setDeferredOfferSelector (toNSNumber value)

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams setVideoStreamIDSelector (toNSNumber value)

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferResponseParams setAudioStreamIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRWebRTCTransportProviderClusterSolicitOfferResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector '[] (Id NSNumber)
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector '[Id NSNumber] ()
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @deferredOffer@
deferredOfferSelector :: Selector '[] (Id NSNumber)
deferredOfferSelector = mkSelector "deferredOffer"

-- | @Selector@ for @setDeferredOffer:@
setDeferredOfferSelector :: Selector '[Id NSNumber] ()
setDeferredOfferSelector = mkSelector "setDeferredOffer:"

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

