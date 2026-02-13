{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
captions avCaptionRenderer =
  sendMessage avCaptionRenderer captionsSelector

-- | captions
--
-- A NSArray holding captions to consider for rendering.
--
-- This is the array of AVCaptions to consider when drawing. The array can contain no captions.
--
-- ObjC selector: @- setCaptions:@
setCaptions :: (IsAVCaptionRenderer avCaptionRenderer, IsNSArray value) => avCaptionRenderer -> value -> IO ()
setCaptions avCaptionRenderer value =
  sendMessage avCaptionRenderer setCaptionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @captions@
captionsSelector :: Selector '[] (Id NSArray)
captionsSelector = mkSelector "captions"

-- | @Selector@ for @setCaptions:@
setCaptionsSelector :: Selector '[Id NSArray] ()
setCaptionsSelector = mkSelector "setCaptions:"

