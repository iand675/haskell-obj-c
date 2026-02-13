{-# LANGUAGE DataKinds #-}
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
  , classifyingLabelSelector
  , itemsSelector
  , uniqueIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- items@
items :: IsAVMetadataGroup avMetadataGroup => avMetadataGroup -> IO (Id NSArray)
items avMetadataGroup =
  sendMessage avMetadataGroup itemsSelector

-- | @- classifyingLabel@
classifyingLabel :: IsAVMetadataGroup avMetadataGroup => avMetadataGroup -> IO (Id NSString)
classifyingLabel avMetadataGroup =
  sendMessage avMetadataGroup classifyingLabelSelector

-- | @- uniqueID@
uniqueID :: IsAVMetadataGroup avMetadataGroup => avMetadataGroup -> IO (Id NSString)
uniqueID avMetadataGroup =
  sendMessage avMetadataGroup uniqueIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @classifyingLabel@
classifyingLabelSelector :: Selector '[] (Id NSString)
classifyingLabelSelector = mkSelector "classifyingLabel"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] (Id NSString)
uniqueIDSelector = mkSelector "uniqueID"

