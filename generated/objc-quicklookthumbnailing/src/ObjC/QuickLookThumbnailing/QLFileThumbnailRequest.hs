{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class contains information about the thumbnail that should be provided.
--
-- Generated bindings for @QLFileThumbnailRequest@.
module ObjC.QuickLookThumbnailing.QLFileThumbnailRequest
  ( QLFileThumbnailRequest
  , IsQLFileThumbnailRequest(..)
  , scale
  , fileURL
  , scaleSelector
  , fileURLSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The scale of the requested thumbnail.
--
-- ObjC selector: @- scale@
scale :: IsQLFileThumbnailRequest qlFileThumbnailRequest => qlFileThumbnailRequest -> IO CDouble
scale qlFileThumbnailRequest  =
  sendMsg qlFileThumbnailRequest (mkSelector "scale") retCDouble []

-- | The url of the file for which a thumbnail is being requested.
--
-- ObjC selector: @- fileURL@
fileURL :: IsQLFileThumbnailRequest qlFileThumbnailRequest => qlFileThumbnailRequest -> IO (Id NSURL)
fileURL qlFileThumbnailRequest  =
  sendMsg qlFileThumbnailRequest (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

