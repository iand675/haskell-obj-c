{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetResourceLoadingRequest
--
-- AVAssetResourceLoadingRequest encapsulates information about a resource request issued by a resource loader.
--
-- When an AVURLAsset needs help loading a resource, it asks its AVAssetResourceLoader object to assist. The resource loader encapsulates the request information by creating an instance of this object, which it then hands to its delegate for processing. The delegate uses the information in this object to perform the request and report on the success or failure of the operation.
--
-- Generated bindings for @AVAssetResourceLoadingRequestInternal@.
module ObjC.AVFoundation.AVAssetResourceLoadingRequestInternal
  ( AVAssetResourceLoadingRequestInternal
  , IsAVAssetResourceLoadingRequestInternal(..)


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

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

