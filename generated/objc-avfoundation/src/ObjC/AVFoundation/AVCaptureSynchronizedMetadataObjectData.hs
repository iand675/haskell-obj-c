{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSynchronizedMetadataObjectData
--
-- An concrete subclass of AVCaptureSynchronizedData representing the data delivered by an AVCaptureMetadataOutput.
--
-- A single AVCaptureMetadataOutput may be configured to deliver multiple kinds of metadata objects (such as QRCodes and detected faces). AVCaptureSynchronizedMetadataObjectData's -metadataObjects array may contain multiple AVMetadataObject subclasses, depending on how the AVCaptureMetadataOutput was configured. All synchronized metadata objects share a common timestamp.
--
-- Generated bindings for @AVCaptureSynchronizedMetadataObjectData@.
module ObjC.AVFoundation.AVCaptureSynchronizedMetadataObjectData
  ( AVCaptureSynchronizedMetadataObjectData
  , IsAVCaptureSynchronizedMetadataObjectData(..)
  , metadataObjects
  , metadataObjectsSelector


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

-- | metadataObjects
--
-- An array of AVMetadataObject subclasses.
--
-- -metadataObjects is never nil. If no metadata objects are present for a given time, an empty array is returned.
--
-- ObjC selector: @- metadataObjects@
metadataObjects :: IsAVCaptureSynchronizedMetadataObjectData avCaptureSynchronizedMetadataObjectData => avCaptureSynchronizedMetadataObjectData -> IO (Id NSArray)
metadataObjects avCaptureSynchronizedMetadataObjectData  =
  sendMsg avCaptureSynchronizedMetadataObjectData (mkSelector "metadataObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataObjects@
metadataObjectsSelector :: Selector
metadataObjectsSelector = mkSelector "metadataObjects"

