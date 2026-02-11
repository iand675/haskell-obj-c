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
items :: IsAVMutableTimedMetadataGroup avMutableTimedMetadataGroup => avMutableTimedMetadataGroup -> IO (Id NSArray)
items avMutableTimedMetadataGroup  =
  sendMsg avMutableTimedMetadataGroup (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setItems:@
setItems :: (IsAVMutableTimedMetadataGroup avMutableTimedMetadataGroup, IsNSArray value) => avMutableTimedMetadataGroup -> value -> IO ()
setItems avMutableTimedMetadataGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableTimedMetadataGroup (mkSelector "setItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @setItems:@
setItemsSelector :: Selector
setItemsSelector = mkSelector "setItems:"

