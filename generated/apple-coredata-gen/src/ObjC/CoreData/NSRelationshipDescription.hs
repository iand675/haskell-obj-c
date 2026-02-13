{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , deleteRuleSelector
  , destinationEntitySelector
  , inverseRelationshipSelector
  , maxCountSelector
  , minCountSelector
  , orderedSelector
  , setDeleteRuleSelector
  , setDestinationEntitySelector
  , setInverseRelationshipSelector
  , setMaxCountSelector
  , setMinCountSelector
  , setOrderedSelector
  , toManySelector
  , versionHashSelector

  -- * Enum types
  , NSDeleteRule(NSDeleteRule)
  , pattern NSNoActionDeleteRule
  , pattern NSNullifyDeleteRule
  , pattern NSCascadeDeleteRule
  , pattern NSDenyDeleteRule

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- destinationEntity@
destinationEntity :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO (Id NSEntityDescription)
destinationEntity nsRelationshipDescription =
  sendMessage nsRelationshipDescription destinationEntitySelector

-- | @- setDestinationEntity:@
setDestinationEntity :: (IsNSRelationshipDescription nsRelationshipDescription, IsNSEntityDescription value) => nsRelationshipDescription -> value -> IO ()
setDestinationEntity nsRelationshipDescription value =
  sendMessage nsRelationshipDescription setDestinationEntitySelector (toNSEntityDescription value)

-- | @- inverseRelationship@
inverseRelationship :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO (Id NSRelationshipDescription)
inverseRelationship nsRelationshipDescription =
  sendMessage nsRelationshipDescription inverseRelationshipSelector

-- | @- setInverseRelationship:@
setInverseRelationship :: (IsNSRelationshipDescription nsRelationshipDescription, IsNSRelationshipDescription value) => nsRelationshipDescription -> value -> IO ()
setInverseRelationship nsRelationshipDescription value =
  sendMessage nsRelationshipDescription setInverseRelationshipSelector (toNSRelationshipDescription value)

-- | @- maxCount@
maxCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO CULong
maxCount nsRelationshipDescription =
  sendMessage nsRelationshipDescription maxCountSelector

-- | @- setMaxCount:@
setMaxCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> CULong -> IO ()
setMaxCount nsRelationshipDescription value =
  sendMessage nsRelationshipDescription setMaxCountSelector value

-- | @- minCount@
minCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO CULong
minCount nsRelationshipDescription =
  sendMessage nsRelationshipDescription minCountSelector

-- | @- setMinCount:@
setMinCount :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> CULong -> IO ()
setMinCount nsRelationshipDescription value =
  sendMessage nsRelationshipDescription setMinCountSelector value

-- | @- deleteRule@
deleteRule :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO NSDeleteRule
deleteRule nsRelationshipDescription =
  sendMessage nsRelationshipDescription deleteRuleSelector

-- | @- setDeleteRule:@
setDeleteRule :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> NSDeleteRule -> IO ()
setDeleteRule nsRelationshipDescription value =
  sendMessage nsRelationshipDescription setDeleteRuleSelector value

-- | @- toMany@
toMany :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO Bool
toMany nsRelationshipDescription =
  sendMessage nsRelationshipDescription toManySelector

-- | @- versionHash@
versionHash :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO (Id NSData)
versionHash nsRelationshipDescription =
  sendMessage nsRelationshipDescription versionHashSelector

-- | @- ordered@
ordered :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> IO Bool
ordered nsRelationshipDescription =
  sendMessage nsRelationshipDescription orderedSelector

-- | @- setOrdered:@
setOrdered :: IsNSRelationshipDescription nsRelationshipDescription => nsRelationshipDescription -> Bool -> IO ()
setOrdered nsRelationshipDescription value =
  sendMessage nsRelationshipDescription setOrderedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @destinationEntity@
destinationEntitySelector :: Selector '[] (Id NSEntityDescription)
destinationEntitySelector = mkSelector "destinationEntity"

-- | @Selector@ for @setDestinationEntity:@
setDestinationEntitySelector :: Selector '[Id NSEntityDescription] ()
setDestinationEntitySelector = mkSelector "setDestinationEntity:"

-- | @Selector@ for @inverseRelationship@
inverseRelationshipSelector :: Selector '[] (Id NSRelationshipDescription)
inverseRelationshipSelector = mkSelector "inverseRelationship"

-- | @Selector@ for @setInverseRelationship:@
setInverseRelationshipSelector :: Selector '[Id NSRelationshipDescription] ()
setInverseRelationshipSelector = mkSelector "setInverseRelationship:"

-- | @Selector@ for @maxCount@
maxCountSelector :: Selector '[] CULong
maxCountSelector = mkSelector "maxCount"

-- | @Selector@ for @setMaxCount:@
setMaxCountSelector :: Selector '[CULong] ()
setMaxCountSelector = mkSelector "setMaxCount:"

-- | @Selector@ for @minCount@
minCountSelector :: Selector '[] CULong
minCountSelector = mkSelector "minCount"

-- | @Selector@ for @setMinCount:@
setMinCountSelector :: Selector '[CULong] ()
setMinCountSelector = mkSelector "setMinCount:"

-- | @Selector@ for @deleteRule@
deleteRuleSelector :: Selector '[] NSDeleteRule
deleteRuleSelector = mkSelector "deleteRule"

-- | @Selector@ for @setDeleteRule:@
setDeleteRuleSelector :: Selector '[NSDeleteRule] ()
setDeleteRuleSelector = mkSelector "setDeleteRule:"

-- | @Selector@ for @toMany@
toManySelector :: Selector '[] Bool
toManySelector = mkSelector "toMany"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector '[] (Id NSData)
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @ordered@
orderedSelector :: Selector '[] Bool
orderedSelector = mkSelector "ordered"

-- | @Selector@ for @setOrdered:@
setOrderedSelector :: Selector '[Bool] ()
setOrderedSelector = mkSelector "setOrdered:"

