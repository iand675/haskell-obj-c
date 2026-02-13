{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetResourceLoadingContentInformationRequest
--
-- An AVAssetResourceLoadingContentInformationRequest represents a query for essential information about a resource referenced by an asset resource loading request.
--
-- When a resource loading delegate accepts responsibility for loading a resource by returning YES from its implementation of resourceLoader:shouldWaitForLoadingOfRequestedResource:, it must check whether the contentInformationRequest property of the AVAssetResourceLoadingRequest is not nil. Whenever the value is not nil, the request includes a query for the information that AVAssetResourceLoadingContentInformationRequest encapsulates. In response to such queries, the resource loading delegate should set the values of the content information request's properties appropriately before invoking the AVAssetResourceLoadingRequest method finishLoading.
--
-- When finishLoading is invoked, the values of the properties of its contentInformationRequest property will, in part, determine how the requested resource is processed. For example, if the requested resource's URL is the URL of an AVURLAsset and contentType is set by the resource loading delegate to a value that the underlying media system doesn't recognize as a supported media file type, operations on the AVURLAsset, such as playback, are likely to fail.
--
-- Generated bindings for @AVAssetResourceLoadingContentInformationRequestInternal@.
module ObjC.AVFoundation.AVAssetResourceLoadingContentInformationRequestInternal
  ( AVAssetResourceLoadingContentInformationRequestInternal
  , IsAVAssetResourceLoadingContentInformationRequestInternal(..)


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

