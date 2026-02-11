{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataGroup
--
-- AVMetadataGroup is the common superclass for AVTimedMetadataGroup and AVDateRangeMetadataGroup; each represents a collection of metadata items associated with a segment of a timeline. AVTimedMetadataGroup is typically used with content that defines an independent timeline, while AVDateRangeMetadataGroup is typically used with content that's associated with a specific range of dates.
--
-- Generated bindings for @AVMetadataGroup@.
module ObjC.AVFoundation.AVMetadataGroup
  ( AVMetadataGroup
  , IsAVMetadataGroup(..)
  , items
  , classifyingLabel
  , uniqueID
  , itemsSelector
  , classifyingLabelSelector
  , uniqueIDSelector


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

-- | @- items@
items :: IsAVMetadataGroup avMetadataGroup => avMetadataGroup -> IO (Id NSArray)
items avMetadataGroup  =
  sendMsg avMetadataGroup (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- classifyingLabel@
classifyingLabel :: IsAVMetadataGroup avMetadataGroup => avMetadataGroup -> IO (Id NSString)
classifyingLabel avMetadataGroup  =
  sendMsg avMetadataGroup (mkSelector "classifyingLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- uniqueID@
uniqueID :: IsAVMetadataGroup avMetadataGroup => avMetadataGroup -> IO (Id NSString)
uniqueID avMetadataGroup  =
  sendMsg avMetadataGroup (mkSelector "uniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @classifyingLabel@
classifyingLabelSelector :: Selector
classifyingLabelSelector = mkSelector "classifyingLabel"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector
uniqueIDSelector = mkSelector "uniqueID"

