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
  , initWithURLSelector
  , valueForAttributeSelector
  , valuesForAttributesSelector
  , attributesSelector


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

-- | @- initWithURL:@
initWithURL :: (IsNSMetadataItem nsMetadataItem, IsNSURL url) => nsMetadataItem -> url -> IO (Id NSMetadataItem)
initWithURL nsMetadataItem  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsMetadataItem (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- valueForAttribute:@
valueForAttribute :: (IsNSMetadataItem nsMetadataItem, IsNSString key) => nsMetadataItem -> key -> IO RawId
valueForAttribute nsMetadataItem  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsMetadataItem (mkSelector "valueForAttribute:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- valuesForAttributes:@
valuesForAttributes :: (IsNSMetadataItem nsMetadataItem, IsNSArray keys) => nsMetadataItem -> keys -> IO (Id NSDictionary)
valuesForAttributes nsMetadataItem  keys =
withObjCPtr keys $ \raw_keys ->
    sendMsg nsMetadataItem (mkSelector "valuesForAttributes:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ())] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsNSMetadataItem nsMetadataItem => nsMetadataItem -> IO (Id NSArray)
attributes nsMetadataItem  =
  sendMsg nsMetadataItem (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @valueForAttribute:@
valueForAttributeSelector :: Selector
valueForAttributeSelector = mkSelector "valueForAttribute:"

-- | @Selector@ for @valuesForAttributes:@
valuesForAttributesSelector :: Selector
valuesForAttributesSelector = mkSelector "valuesForAttributes:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

