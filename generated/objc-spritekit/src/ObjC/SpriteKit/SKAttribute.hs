{-# LANGUAGE PatternSynonyms #-}
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

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ attributeWithName:type:@
attributeWithName_type :: IsNSString name => name -> SKAttributeType -> IO (Id SKAttribute)
attributeWithName_type name type_ =
  do
    cls' <- getRequiredClass "SKAttribute"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "attributeWithName:type:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCLong (coerce type_)] >>= retainedObject . castPtr

-- | @- initWithName:type:@
initWithName_type :: (IsSKAttribute skAttribute, IsNSString name) => skAttribute -> name -> SKAttributeType -> IO (Id SKAttribute)
initWithName_type skAttribute  name type_ =
withObjCPtr name $ \raw_name ->
    sendMsg skAttribute (mkSelector "initWithName:type:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCLong (coerce type_)] >>= ownedObject . castPtr

-- | @- name@
name :: IsSKAttribute skAttribute => skAttribute -> IO (Id NSString)
name skAttribute  =
  sendMsg skAttribute (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsSKAttribute skAttribute => skAttribute -> IO SKAttributeType
type_ skAttribute  =
  fmap (coerce :: CLong -> SKAttributeType) $ sendMsg skAttribute (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeWithName:type:@
attributeWithName_typeSelector :: Selector
attributeWithName_typeSelector = mkSelector "attributeWithName:type:"

-- | @Selector@ for @initWithName:type:@
initWithName_typeSelector :: Selector
initWithName_typeSelector = mkSelector "initWithName:type:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

