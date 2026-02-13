{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableTimedMetadataGroup
--
-- AVMutableTimedMetadataGroup is used to represent a mutable collection of metadata items that are valid for use during a specific range of time.
--
-- Generated bindings for @AVMutableTimedMetadataGroup@.
module ObjC.AVFoundation.AVMutableTimedMetadataGroup
  ( AVMutableTimedMetadataGroup
  , IsAVMutableTimedMetadataGroup(..)
  , items
  , setItems
  , itemsSelector
  , setItemsSelector


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
items :: IsAVMutableTimedMetadataGroup avMutableTimedMetadataGroup => avMutableTimedMetadataGroup -> IO (Id NSArray)
items avMutableTimedMetadataGroup =
  sendMessage avMutableTimedMetadataGroup itemsSelector

-- | @- setItems:@
setItems :: (IsAVMutableTimedMetadataGroup avMutableTimedMetadataGroup, IsNSArray value) => avMutableTimedMetadataGroup -> value -> IO ()
setItems avMutableTimedMetadataGroup value =
  sendMessage avMutableTimedMetadataGroup setItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @setItems:@
setItemsSelector :: Selector '[Id NSArray] ()
setItemsSelector = mkSelector "setItems:"

