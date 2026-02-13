{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMetadataItemValueRequest@.
module ObjC.AVFoundation.AVMetadataItemValueRequest
  ( AVMetadataItemValueRequest
  , IsAVMetadataItemValueRequest(..)
  , respondWithValue
  , respondWithError
  , metadataItem
  , metadataItemSelector
  , respondWithErrorSelector
  , respondWithValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | respondWithValue:
--
-- Allows you to respond to an AVMetadataItemValueRequest by providing a value.
--
-- @value@ — The value of the AVMetadataItem.
--
-- ObjC selector: @- respondWithValue:@
respondWithValue :: IsAVMetadataItemValueRequest avMetadataItemValueRequest => avMetadataItemValueRequest -> RawId -> IO ()
respondWithValue avMetadataItemValueRequest value =
  sendMessage avMetadataItemValueRequest respondWithValueSelector value

-- | respondWithError:
--
-- Allows you to respond to an AVMetadataItemValueRequest in the case of failure.
--
-- @error@ — An instance of NSError that describes a failure encountered while loading the value of an AVMetadataItem.
--
-- ObjC selector: @- respondWithError:@
respondWithError :: (IsAVMetadataItemValueRequest avMetadataItemValueRequest, IsNSError error_) => avMetadataItemValueRequest -> error_ -> IO ()
respondWithError avMetadataItemValueRequest error_ =
  sendMessage avMetadataItemValueRequest respondWithErrorSelector (toNSError error_)

-- | @- metadataItem@
metadataItem :: IsAVMetadataItemValueRequest avMetadataItemValueRequest => avMetadataItemValueRequest -> IO (Id AVMetadataItem)
metadataItem avMetadataItemValueRequest =
  sendMessage avMetadataItemValueRequest metadataItemSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @respondWithValue:@
respondWithValueSelector :: Selector '[RawId] ()
respondWithValueSelector = mkSelector "respondWithValue:"

-- | @Selector@ for @respondWithError:@
respondWithErrorSelector :: Selector '[Id NSError] ()
respondWithErrorSelector = mkSelector "respondWithError:"

-- | @Selector@ for @metadataItem@
metadataItemSelector :: Selector '[] (Id AVMetadataItem)
metadataItemSelector = mkSelector "metadataItem"

