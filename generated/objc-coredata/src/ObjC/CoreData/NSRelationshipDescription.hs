{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRelationshipDescription@.
module ObjC.CoreData.NSRelationshipDescription
  ( NSRelationshipDescription
  , IsNSRelationshipDescription(..)
  , destinationEntity
  , setDestinationEntity
  , inverseRelationship
  , setInverseRelationship
  , maxCount
  , setMaxCount
  , minCount
  , setMinCount
  , deleteRule
  , setDeleteRule
  , toMany
  , versionHash
  , ordered
  , setOrdered
  , destinationEntitySelector
  , setDestinationEntitySelector
  , inverseRelationshipSelector
  , setInverseRelationshipSelector
  , maxCountSelector
  , setMaxCountSelector
  , minCountSelector
  , setMinCountSelector
  , deleteRuleSelector
  , setDeleteRuleSelector
  , toManySelector
  , versionHashSelector
  , orderedSelector
  , setOrderedSelector

  -- * Enum types
  , NSDeleteRule(NSDeleteRule)
  , pattern NSNoActionDeleteRule
  , pattern NSNullifyDeleteRule
  , pattern NSCascadeDeleteRule
  , pattern NSDenyDeleteRule

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- destinationEntity@
destinationEntity :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO (Id NSEntityDescription)
destinationEntity nsRelationshipDescription  =
  sendMsg nsRelationshipDescription (mkSelector "destinationEntity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestinationEntity:@
setDestinationEntity :: (IsNSRelationshipDescription nsRelationshipDescription, IsNSEntityDescription value) => nsRelationshipDescription -> value -> IO ()
setDestinationEntity nsRelationshipDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRelationshipDescription (mkSelector "setDestinationEntity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inverseRelationship@
inverseRelationship :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO (Id NSRelationshipDescription)
inverseRelationship nsRelationshipDescription  =
  sendMsg nsRelationshipDescription (mkSelector "inverseRelationship") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInverseRelationship:@
setInverseRelationship :: (IsNSRelationshipDescription nsRelationshipDescription, IsNSRelationshipDescription value) => nsRelationshipDescription -> value -> IO ()
setInverseRelationship nsRelationshipDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRelationshipDescription (mkSelector "setInverseRelationship:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxCount@
maxCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO CULong
maxCount nsRelationshipDescription  =
  sendMsg nsRelationshipDescription (mkSelector "maxCount") retCULong []

-- | @- setMaxCount:@
setMaxCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> CULong -> IO ()
setMaxCount nsRelationshipDescription  value =
  sendMsg nsRelationshipDescription (mkSelector "setMaxCount:") retVoid [argCULong (fromIntegral value)]

-- | @- minCount@
minCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO CULong
minCount nsRelationshipDescription  =
  sendMsg nsRelationshipDescription (mkSelector "minCount") retCULong []

-- | @- setMinCount:@
setMinCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> CULong -> IO ()
setMinCount nsRelationshipDescription  value =
  sendMsg nsRelationshipDescription (mkSelector "setMinCount:") retVoid [argCULong (fromIntegral value)]

-- | @- deleteRule@
deleteRule :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO NSDeleteRule
deleteRule nsRelationshipDescription  =
  fmap (coerce :: CULong -> NSDeleteRule) $ sendMsg nsRelationshipDescription (mkSelector "deleteRule") retCULong []

-- | @- setDeleteRule:@
setDeleteRule :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> NSDeleteRule -> IO ()
setDeleteRule nsRelationshipDescription  value =
  sendMsg nsRelationshipDescription (mkSelector "setDeleteRule:") retVoid [argCULong (coerce value)]

-- | @- toMany@
toMany :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO Bool
toMany nsRelationshipDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRelationshipDescription (mkSelector "toMany") retCULong []

-- | @- versionHash@
versionHash :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO (Id NSData)
versionHash nsRelationshipDescription  =
  sendMsg nsRelationshipDescription (mkSelector "versionHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ordered@
ordered :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO Bool
ordered nsRelationshipDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRelationshipDescription (mkSelector "ordered") retCULong []

-- | @- setOrdered:@
setOrdered :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> Bool -> IO ()
setOrdered nsRelationshipDescription  value =
  sendMsg nsRelationshipDescription (mkSelector "setOrdered:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @destinationEntity@
destinationEntitySelector :: Selector
destinationEntitySelector = mkSelector "destinationEntity"

-- | @Selector@ for @setDestinationEntity:@
setDestinationEntitySelector :: Selector
setDestinationEntitySelector = mkSelector "setDestinationEntity:"

-- | @Selector@ for @inverseRelationship@
inverseRelationshipSelector :: Selector
inverseRelationshipSelector = mkSelector "inverseRelationship"

-- | @Selector@ for @setInverseRelationship:@
setInverseRelationshipSelector :: Selector
setInverseRelationshipSelector = mkSelector "setInverseRelationship:"

-- | @Selector@ for @maxCount@
maxCountSelector :: Selector
maxCountSelector = mkSelector "maxCount"

-- | @Selector@ for @setMaxCount:@
setMaxCountSelector :: Selector
setMaxCountSelector = mkSelector "setMaxCount:"

-- | @Selector@ for @minCount@
minCountSelector :: Selector
minCountSelector = mkSelector "minCount"

-- | @Selector@ for @setMinCount:@
setMinCountSelector :: Selector
setMinCountSelector = mkSelector "setMinCount:"

-- | @Selector@ for @deleteRule@
deleteRuleSelector :: Selector
deleteRuleSelector = mkSelector "deleteRule"

-- | @Selector@ for @setDeleteRule:@
setDeleteRuleSelector :: Selector
setDeleteRuleSelector = mkSelector "setDeleteRule:"

-- | @Selector@ for @toMany@
toManySelector :: Selector
toManySelector = mkSelector "toMany"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @ordered@
orderedSelector :: Selector
orderedSelector = mkSelector "ordered"

-- | @Selector@ for @setOrdered:@
setOrderedSelector :: Selector
setOrderedSelector = mkSelector "setOrdered:"

