{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMetadataQueryAttributeValueTuple@.
module ObjC.Foundation.NSMetadataQueryAttributeValueTuple
  ( NSMetadataQueryAttributeValueTuple
  , IsNSMetadataQueryAttributeValueTuple(..)
  , attribute
  , value
  , count
  , attributeSelector
  , valueSelector
  , countSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- attribute@
attribute :: IsNSMetadataQueryAttributeValueTuple nsMetadataQueryAttributeValueTuple => nsMetadataQueryAttributeValueTuple -> IO (Id NSString)
attribute nsMetadataQueryAttributeValueTuple  =
  sendMsg nsMetadataQueryAttributeValueTuple (mkSelector "attribute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- value@
value :: IsNSMetadataQueryAttributeValueTuple nsMetadataQueryAttributeValueTuple => nsMetadataQueryAttributeValueTuple -> IO RawId
value nsMetadataQueryAttributeValueTuple  =
  fmap (RawId . castPtr) $ sendMsg nsMetadataQueryAttributeValueTuple (mkSelector "value") (retPtr retVoid) []

-- | @- count@
count :: IsNSMetadataQueryAttributeValueTuple nsMetadataQueryAttributeValueTuple => nsMetadataQueryAttributeValueTuple -> IO CULong
count nsMetadataQueryAttributeValueTuple  =
  sendMsg nsMetadataQueryAttributeValueTuple (mkSelector "count") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attribute@
attributeSelector :: Selector
attributeSelector = mkSelector "attribute"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

