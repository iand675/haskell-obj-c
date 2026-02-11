{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionRenderer
--
-- An instance of AVCaptionRenderer represents a service that can render the captions for a particular time
--
-- An instance of AVCaptionRenderer performs drawing of a caption "scene" from a population of captions given a time. If there are no captions or no captions at the specified time, "emptiness" will still be drawn (e.g., flood filling with zero alpha or a color).
--
-- Generated bindings for @AVCaptionRenderer@.
module ObjC.AVFoundation.AVCaptionRenderer
  ( AVCaptionRenderer
  , IsAVCaptionRenderer(..)
  , captions
  , setCaptions
  , captionsSelector
  , setCaptionsSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | captions
--
-- A NSArray holding captions to consider for rendering.
--
-- This is the array of AVCaptions to consider when drawing. The array can contain no captions.
--
-- ObjC selector: @- captions@
captions :: IsAVCaptionRenderer avCaptionRenderer => avCaptionRenderer -> IO (Id NSArray)
captions avCaptionRenderer  =
  sendMsg avCaptionRenderer (mkSelector "captions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | captions
--
-- A NSArray holding captions to consider for rendering.
--
-- This is the array of AVCaptions to consider when drawing. The array can contain no captions.
--
-- ObjC selector: @- setCaptions:@
setCaptions :: (IsAVCaptionRenderer avCaptionRenderer, IsNSArray value) => avCaptionRenderer -> value -> IO ()
setCaptions avCaptionRenderer  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptionRenderer (mkSelector "setCaptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @captions@
captionsSelector :: Selector
captionsSelector = mkSelector "captions"

-- | @Selector@ for @setCaptions:@
setCaptionsSelector :: Selector
setCaptionsSelector = mkSelector "setCaptions:"

