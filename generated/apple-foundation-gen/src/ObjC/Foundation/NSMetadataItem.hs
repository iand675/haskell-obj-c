{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMetadataItem@.
module ObjC.Foundation.NSMetadataItem
  ( NSMetadataItem
  , IsNSMetadataItem(..)
  , initWithURL
  , valueForAttribute
  , valuesForAttributes
  , attributes
  , attributesSelector
  , initWithURLSelector
  , valueForAttributeSelector
  , valuesForAttributesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithURL:@
initWithURL :: (IsNSMetadataItem nsMetadataItem, IsNSURL url) => nsMetadataItem -> url -> IO (Id NSMetadataItem)
initWithURL nsMetadataItem url =
  sendOwnedMessage nsMetadataItem initWithURLSelector (toNSURL url)

-- | @- valueForAttribute:@
valueForAttribute :: (IsNSMetadataItem nsMetadataItem, IsNSString key) => nsMetadataItem -> key -> IO RawId
valueForAttribute nsMetadataItem key =
  sendMessage nsMetadataItem valueForAttributeSelector (toNSString key)

-- | @- valuesForAttributes:@
valuesForAttributes :: (IsNSMetadataItem nsMetadataItem, IsNSArray keys) => nsMetadataItem -> keys -> IO (Id NSDictionary)
valuesForAttributes nsMetadataItem keys =
  sendMessage nsMetadataItem valuesForAttributesSelector (toNSArray keys)

-- | @- attributes@
attributes :: IsNSMetadataItem nsMetadataItem => nsMetadataItem -> IO (Id NSArray)
attributes nsMetadataItem =
  sendMessage nsMetadataItem attributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id NSMetadataItem)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @valueForAttribute:@
valueForAttributeSelector :: Selector '[Id NSString] RawId
valueForAttributeSelector = mkSelector "valueForAttribute:"

-- | @Selector@ for @valuesForAttributes:@
valuesForAttributesSelector :: Selector '[Id NSArray] (Id NSDictionary)
valuesForAttributesSelector = mkSelector "valuesForAttributes:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSArray)
attributesSelector = mkSelector "attributes"

