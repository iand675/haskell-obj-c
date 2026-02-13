{-# LANGUAGE DataKinds #-}
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
  , countSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- attribute@
attribute :: IsNSMetadataQueryAttributeValueTuple nsMetadataQueryAttributeValueTuple => nsMetadataQueryAttributeValueTuple -> IO (Id NSString)
attribute nsMetadataQueryAttributeValueTuple =
  sendMessage nsMetadataQueryAttributeValueTuple attributeSelector

-- | @- value@
value :: IsNSMetadataQueryAttributeValueTuple nsMetadataQueryAttributeValueTuple => nsMetadataQueryAttributeValueTuple -> IO RawId
value nsMetadataQueryAttributeValueTuple =
  sendMessage nsMetadataQueryAttributeValueTuple valueSelector

-- | @- count@
count :: IsNSMetadataQueryAttributeValueTuple nsMetadataQueryAttributeValueTuple => nsMetadataQueryAttributeValueTuple -> IO CULong
count nsMetadataQueryAttributeValueTuple =
  sendMessage nsMetadataQueryAttributeValueTuple countSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attribute@
attributeSelector :: Selector '[] (Id NSString)
attributeSelector = mkSelector "attribute"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

