{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKAttribute@.
module ObjC.SpriteKit.SKAttribute
  ( SKAttribute
  , IsSKAttribute(..)
  , attributeWithName_type
  , initWithName_type
  , name
  , type_
  , attributeWithName_typeSelector
  , initWithName_typeSelector
  , nameSelector
  , typeSelector

  -- * Enum types
  , SKAttributeType(SKAttributeType)
  , pattern SKAttributeTypeNone
  , pattern SKAttributeTypeFloat
  , pattern SKAttributeTypeVectorFloat2
  , pattern SKAttributeTypeVectorFloat3
  , pattern SKAttributeTypeVectorFloat4
  , pattern SKAttributeTypeHalfFloat
  , pattern SKAttributeTypeVectorHalfFloat2
  , pattern SKAttributeTypeVectorHalfFloat3
  , pattern SKAttributeTypeVectorHalfFloat4

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ attributeWithName:type:@
attributeWithName_type :: IsNSString name => name -> SKAttributeType -> IO (Id SKAttribute)
attributeWithName_type name type_ =
  do
    cls' <- getRequiredClass "SKAttribute"
    sendClassMessage cls' attributeWithName_typeSelector (toNSString name) type_

-- | @- initWithName:type:@
initWithName_type :: (IsSKAttribute skAttribute, IsNSString name) => skAttribute -> name -> SKAttributeType -> IO (Id SKAttribute)
initWithName_type skAttribute name type_ =
  sendOwnedMessage skAttribute initWithName_typeSelector (toNSString name) type_

-- | @- name@
name :: IsSKAttribute skAttribute => skAttribute -> IO (Id NSString)
name skAttribute =
  sendMessage skAttribute nameSelector

-- | @- type@
type_ :: IsSKAttribute skAttribute => skAttribute -> IO SKAttributeType
type_ skAttribute =
  sendMessage skAttribute typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeWithName:type:@
attributeWithName_typeSelector :: Selector '[Id NSString, SKAttributeType] (Id SKAttribute)
attributeWithName_typeSelector = mkSelector "attributeWithName:type:"

-- | @Selector@ for @initWithName:type:@
initWithName_typeSelector :: Selector '[Id NSString, SKAttributeType] (Id SKAttribute)
initWithName_typeSelector = mkSelector "initWithName:type:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] SKAttributeType
typeSelector = mkSelector "type"

