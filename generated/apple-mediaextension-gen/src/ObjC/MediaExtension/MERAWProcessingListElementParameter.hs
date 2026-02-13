{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MERAWProcessingListElementParameter
--
-- An object implementing this protocol is implemented by the RAW Processor to describe each processing parameter the Processor exposes.
--
-- The MERAWProcessingListElementParameter protocol provides an interface for VideoToolbox to query descriptions of the different elements in a parameter list  for a List element in a MERAWProcessingParameter.  A distinct MERAWProcessingListElementParameter is created for each list element.
--
-- Generated bindings for @MERAWProcessingListElementParameter@.
module ObjC.MediaExtension.MERAWProcessingListElementParameter
  ( MERAWProcessingListElementParameter
  , IsMERAWProcessingListElementParameter(..)
  , initWithName_description_elementID
  , listElementID
  , initWithName_description_elementIDSelector
  , listElementIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:description:elementID:@
initWithName_description_elementID :: (IsMERAWProcessingListElementParameter merawProcessingListElementParameter, IsNSString name, IsNSString description) => merawProcessingListElementParameter -> name -> description -> CLong -> IO (Id MERAWProcessingListElementParameter)
initWithName_description_elementID merawProcessingListElementParameter name description elementID =
  sendOwnedMessage merawProcessingListElementParameter initWithName_description_elementIDSelector (toNSString name) (toNSString description) elementID

-- | listElementID
--
-- A unique number in this list which represents this list option.
--
-- The set of elements in the list may change depending on other configuration parameters, so while the index of an element in this list may change, this ID never changes and is used to report list element selection
--
-- ObjC selector: @- listElementID@
listElementID :: IsMERAWProcessingListElementParameter merawProcessingListElementParameter => merawProcessingListElementParameter -> IO CLong
listElementID merawProcessingListElementParameter =
  sendMessage merawProcessingListElementParameter listElementIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:description:elementID:@
initWithName_description_elementIDSelector :: Selector '[Id NSString, Id NSString, CLong] (Id MERAWProcessingListElementParameter)
initWithName_description_elementIDSelector = mkSelector "initWithName:description:elementID:"

-- | @Selector@ for @listElementID@
listElementIDSelector :: Selector '[] CLong
listElementIDSelector = mkSelector "listElementID"

