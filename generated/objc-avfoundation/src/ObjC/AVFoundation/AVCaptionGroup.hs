{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionGroup
--
-- An instance of AVCaptionGroup represents zero or more captions that intersect in time.
--
-- The time range of each caption may overlap as there can be more than one active caption at a time. A sequence of AVCaptionGroup objects represents such overlapping caption timeline.
--
-- An instance of AVCaptionGroup has a time range and a list of active captions for the time range. Two successive AVCaptionGroup objects have contiguous and non-overlapping time ranges. A new AVCaptionGroup time range commences whenever any of caption becomes active or inactive. When a caption spans over multiple AVCaptionGroup time ranges, these  AVCaptionGroup objects refer to an equal AVCaption object.
--
-- An empty AVCaptionGroup represents the time range without any active captions.
--
-- The list of captions in the group is ordered according to the document order. For example, suppose a TTML document has two temporally overhapping captions:
--
-- Hello      World
--
-- AVCaptionGroup for time range 1s to 2s has the list of captions: Hello and World in this order despite the fact that "World" is shown earlier than "Hello".
--
-- A client may use AVCaptionGroup to get the list of active captions for the time range. For example, presentation processing may find the AVCaptionGroup object for the current time, get the list of captions, and place them into the destination display region.
--
-- Generated bindings for @AVCaptionGroup@.
module ObjC.AVFoundation.AVCaptionGroup
  ( AVCaptionGroup
  , IsAVCaptionGroup(..)
  , captions
  , captionsSelector


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

-- | captions
--
-- An array of AVCaption objects.
--
-- If the value is an empty array, the caption group represents a region of the timeline in which there are no captions.
--
-- ObjC selector: @- captions@
captions :: IsAVCaptionGroup avCaptionGroup => avCaptionGroup -> IO (Id NSArray)
captions avCaptionGroup  =
  sendMsg avCaptionGroup (mkSelector "captions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @captions@
captionsSelector :: Selector
captionsSelector = mkSelector "captions"

