{-# LANGUAGE DataKinds #-}
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
  , attributeKeysSelector
  , classDescriptionForClassSelector
  , invalidateClassDescriptionCacheSelector
  , inverseForRelationshipKeySelector
  , registerClassDescription_forClassSelector
  , toManyRelationshipKeysSelector
  , toOneRelationshipKeysSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ registerClassDescription:forClass:@
registerClassDescription_forClass :: IsNSClassDescription description => description -> Class -> IO ()
registerClassDescription_forClass description aClass =
  do
    cls' <- getRequiredClass "NSClassDescription"
    sendClassMessage cls' registerClassDescription_forClassSelector (toNSClassDescription description) aClass

-- | @+ invalidateClassDescriptionCache@
invalidateClassDescriptionCache :: IO ()
invalidateClassDescriptionCache  =
  do
    cls' <- getRequiredClass "NSClassDescription"
    sendClassMessage cls' invalidateClassDescriptionCacheSelector

-- | @+ classDescriptionForClass:@
classDescriptionForClass :: Class -> IO (Id NSClassDescription)
classDescriptionForClass aClass =
  do
    cls' <- getRequiredClass "NSClassDescription"
    sendClassMessage cls' classDescriptionForClassSelector aClass

-- | @- inverseForRelationshipKey:@
inverseForRelationshipKey :: (IsNSClassDescription nsClassDescription, IsNSString relationshipKey) => nsClassDescription -> relationshipKey -> IO (Id NSString)
inverseForRelationshipKey nsClassDescription relationshipKey =
  sendMessage nsClassDescription inverseForRelationshipKeySelector (toNSString relationshipKey)

-- | @- attributeKeys@
attributeKeys :: IsNSClassDescription nsClassDescription => nsClassDescription -> IO (Id NSArray)
attributeKeys nsClassDescription =
  sendMessage nsClassDescription attributeKeysSelector

-- | @- toOneRelationshipKeys@
toOneRelationshipKeys :: IsNSClassDescription nsClassDescription => nsClassDescription -> IO (Id NSArray)
toOneRelationshipKeys nsClassDescription =
  sendMessage nsClassDescription toOneRelationshipKeysSelector

-- | @- toManyRelationshipKeys@
toManyRelationshipKeys :: IsNSClassDescription nsClassDescription => nsClassDescription -> IO (Id NSArray)
toManyRelationshipKeys nsClassDescription =
  sendMessage nsClassDescription toManyRelationshipKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerClassDescription:forClass:@
registerClassDescription_forClassSelector :: Selector '[Id NSClassDescription, Class] ()
registerClassDescription_forClassSelector = mkSelector "registerClassDescription:forClass:"

-- | @Selector@ for @invalidateClassDescriptionCache@
invalidateClassDescriptionCacheSelector :: Selector '[] ()
invalidateClassDescriptionCacheSelector = mkSelector "invalidateClassDescriptionCache"

-- | @Selector@ for @classDescriptionForClass:@
classDescriptionForClassSelector :: Selector '[Class] (Id NSClassDescription)
classDescriptionForClassSelector = mkSelector "classDescriptionForClass:"

-- | @Selector@ for @inverseForRelationshipKey:@
inverseForRelationshipKeySelector :: Selector '[Id NSString] (Id NSString)
inverseForRelationshipKeySelector = mkSelector "inverseForRelationshipKey:"

-- | @Selector@ for @attributeKeys@
attributeKeysSelector :: Selector '[] (Id NSArray)
attributeKeysSelector = mkSelector "attributeKeys"

-- | @Selector@ for @toOneRelationshipKeys@
toOneRelationshipKeysSelector :: Selector '[] (Id NSArray)
toOneRelationshipKeysSelector = mkSelector "toOneRelationshipKeys"

-- | @Selector@ for @toManyRelationshipKeys@
toManyRelationshipKeysSelector :: Selector '[] (Id NSArray)
toManyRelationshipKeysSelector = mkSelector "toManyRelationshipKeys"

