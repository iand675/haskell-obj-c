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
  , respondWithValueSelector
  , respondWithErrorSelector
  , metadataItemSelector


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
import ObjC.Foundation.Internal.Classes

-- | respondWithValue:
--
-- Allows you to respond to an AVMetadataItemValueRequest by providing a value.
--
-- @value@ — The value of the AVMetadataItem.
--
-- ObjC selector: @- respondWithValue:@
respondWithValue :: IsAVMetadataItemValueRequest avMetadataItemValueRequest => avMetadataItemValueRequest -> RawId -> IO ()
respondWithValue avMetadataItemValueRequest  value =
  sendMsg avMetadataItemValueRequest (mkSelector "respondWithValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | respondWithError:
--
-- Allows you to respond to an AVMetadataItemValueRequest in the case of failure.
--
-- @error@ — An instance of NSError that describes a failure encountered while loading the value of an AVMetadataItem.
--
-- ObjC selector: @- respondWithError:@
respondWithError :: (IsAVMetadataItemValueRequest avMetadataItemValueRequest, IsNSError error_) => avMetadataItemValueRequest -> error_ -> IO ()
respondWithError avMetadataItemValueRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg avMetadataItemValueRequest (mkSelector "respondWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- metadataItem@
metadataItem :: IsAVMetadataItemValueRequest avMetadataItemValueRequest => avMetadataItemValueRequest -> IO (Id AVMetadataItem)
metadataItem avMetadataItemValueRequest  =
  sendMsg avMetadataItemValueRequest (mkSelector "metadataItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @respondWithValue:@
respondWithValueSelector :: Selector
respondWithValueSelector = mkSelector "respondWithValue:"

-- | @Selector@ for @respondWithError:@
respondWithErrorSelector :: Selector
respondWithErrorSelector = mkSelector "respondWithError:"

-- | @Selector@ for @metadataItem@
metadataItemSelector :: Selector
metadataItemSelector = mkSelector "metadataItem"

