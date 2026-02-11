{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSClassDescription@.
module ObjC.Foundation.NSClassDescription
  ( NSClassDescription
  , IsNSClassDescription(..)
  , registerClassDescription_forClass
  , invalidateClassDescriptionCache
  , classDescriptionForClass
  , inverseForRelationshipKey
  , attributeKeys
  , toOneRelationshipKeys
  , toManyRelationshipKeys
  , registerClassDescription_forClassSelector
  , invalidateClassDescriptionCacheSelector
  , classDescriptionForClassSelector
  , inverseForRelationshipKeySelector
  , attributeKeysSelector
  , toOneRelationshipKeysSelector
  , toManyRelationshipKeysSelector


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

-- | @+ registerClassDescription:forClass:@
registerClassDescription_forClass :: IsNSClassDescription description => description -> Class -> IO ()
registerClassDescription_forClass description aClass =
  do
    cls' <- getRequiredClass "NSClassDescription"
    withObjCPtr description $ \raw_description ->
      sendClassMsg cls' (mkSelector "registerClassDescription:forClass:") retVoid [argPtr (castPtr raw_description :: Ptr ()), argPtr (unClass aClass)]

-- | @+ invalidateClassDescriptionCache@
invalidateClassDescriptionCache :: IO ()
invalidateClassDescriptionCache  =
  do
    cls' <- getRequiredClass "NSClassDescription"
    sendClassMsg cls' (mkSelector "invalidateClassDescriptionCache") retVoid []

-- | @+ classDescriptionForClass:@
classDescriptionForClass :: Class -> IO (Id NSClassDescription)
classDescriptionForClass aClass =
  do
    cls' <- getRequiredClass "NSClassDescription"
    sendClassMsg cls' (mkSelector "classDescriptionForClass:") (retPtr retVoid) [argPtr (unClass aClass)] >>= retainedObject . castPtr

-- | @- inverseForRelationshipKey:@
inverseForRelationshipKey :: (IsNSClassDescription nsClassDescription, IsNSString relationshipKey) => nsClassDescription -> relationshipKey -> IO (Id NSString)
inverseForRelationshipKey nsClassDescription  relationshipKey =
withObjCPtr relationshipKey $ \raw_relationshipKey ->
    sendMsg nsClassDescription (mkSelector "inverseForRelationshipKey:") (retPtr retVoid) [argPtr (castPtr raw_relationshipKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- attributeKeys@
attributeKeys :: IsNSClassDescription nsClassDescription => nsClassDescription -> IO (Id NSArray)
attributeKeys nsClassDescription  =
  sendMsg nsClassDescription (mkSelector "attributeKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toOneRelationshipKeys@
toOneRelationshipKeys :: IsNSClassDescription nsClassDescription => nsClassDescription -> IO (Id NSArray)
toOneRelationshipKeys nsClassDescription  =
  sendMsg nsClassDescription (mkSelector "toOneRelationshipKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toManyRelationshipKeys@
toManyRelationshipKeys :: IsNSClassDescription nsClassDescription => nsClassDescription -> IO (Id NSArray)
toManyRelationshipKeys nsClassDescription  =
  sendMsg nsClassDescription (mkSelector "toManyRelationshipKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerClassDescription:forClass:@
registerClassDescription_forClassSelector :: Selector
registerClassDescription_forClassSelector = mkSelector "registerClassDescription:forClass:"

-- | @Selector@ for @invalidateClassDescriptionCache@
invalidateClassDescriptionCacheSelector :: Selector
invalidateClassDescriptionCacheSelector = mkSelector "invalidateClassDescriptionCache"

-- | @Selector@ for @classDescriptionForClass:@
classDescriptionForClassSelector :: Selector
classDescriptionForClassSelector = mkSelector "classDescriptionForClass:"

-- | @Selector@ for @inverseForRelationshipKey:@
inverseForRelationshipKeySelector :: Selector
inverseForRelationshipKeySelector = mkSelector "inverseForRelationshipKey:"

-- | @Selector@ for @attributeKeys@
attributeKeysSelector :: Selector
attributeKeysSelector = mkSelector "attributeKeys"

-- | @Selector@ for @toOneRelationshipKeys@
toOneRelationshipKeysSelector :: Selector
toOneRelationshipKeysSelector = mkSelector "toOneRelationshipKeys"

-- | @Selector@ for @toManyRelationshipKeys@
toManyRelationshipKeysSelector :: Selector
toManyRelationshipKeysSelector = mkSelector "toManyRelationshipKeys"

