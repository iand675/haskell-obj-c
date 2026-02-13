{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetResourceLoadingDataRequest
--
-- An AVAssetResourceLoadingDataRequest is used to request data from a resource referenced by an AVAssetResourceLoadingRequest.
--
-- The AVAssetResourceLoaderDelegate uses the AVAssetResourceLoadingDataRequest class to do the actual data reading, and its methods will be invoked, as necessary, to acquire data for the AVAssetResourceLoadingRequest instance.
--
-- When a resource loading delegate accepts responsibility for loading a resource by returning YES from its implementation of resourceLoader:shouldWaitForLoadingOfRequestedResource:, it must check whether the dataRequest property of the AVAssetResourceLoadingRequest instance is not nil. If it is not nil, the resource loading delegate is informed of the range of bytes within the resource that are required by the underlying media system. In response, the data is provided by one or more invocations of respondWithData: as needed for provision of the requested data. The data can be provided in increments determined by the resource loading delegate according to convenience or efficiency.
--
-- When the AVAssetResourceLoadingRequest method finishLoading is invoked, the data request is considered fully satisfied. If the entire range of bytes requested has not yet been provided, the underlying media system assumes that the resource's length is limited to the provided content.
--
-- Generated bindings for @AVAssetResourceLoadingDataRequestInternal@.
module ObjC.AVFoundation.AVAssetResourceLoadingDataRequestInternal
  ( AVAssetResourceLoadingDataRequestInternal
  , IsAVAssetResourceLoadingDataRequestInternal(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

