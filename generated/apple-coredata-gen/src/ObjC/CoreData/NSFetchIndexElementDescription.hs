{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFetchIndexElementDescription@.
module ObjC.CoreData.NSFetchIndexElementDescription
  ( NSFetchIndexElementDescription
  , IsNSFetchIndexElementDescription(..)
  , initWithProperty_collationType
  , property
  , propertyName
  , collationType
  , setCollationType
  , ascending
  , setAscending
  , indexDescription
  , ascendingSelector
  , collationTypeSelector
  , indexDescriptionSelector
  , initWithProperty_collationTypeSelector
  , propertyNameSelector
  , propertySelector
  , setAscendingSelector
  , setCollationTypeSelector

  -- * Enum types
  , NSFetchIndexElementType(NSFetchIndexElementType)
  , pattern NSFetchIndexElementTypeBinary
  , pattern NSFetchIndexElementTypeRTree

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

-- | @- initWithProperty:collationType:@
initWithProperty_collationType :: (IsNSFetchIndexElementDescription nsFetchIndexElementDescription, IsNSPropertyDescription property) => nsFetchIndexElementDescription -> property -> NSFetchIndexElementType -> IO (Id NSFetchIndexElementDescription)
initWithProperty_collationType nsFetchIndexElementDescription property collationType =
  sendOwnedMessage nsFetchIndexElementDescription initWithProperty_collationTypeSelector (toNSPropertyDescription property) collationType

-- | @- property@
property :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO (Id NSPropertyDescription)
property nsFetchIndexElementDescription =
  sendMessage nsFetchIndexElementDescription propertySelector

-- | @- propertyName@
propertyName :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO (Id NSString)
propertyName nsFetchIndexElementDescription =
  sendMessage nsFetchIndexElementDescription propertyNameSelector

-- | @- collationType@
collationType :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO NSFetchIndexElementType
collationType nsFetchIndexElementDescription =
  sendMessage nsFetchIndexElementDescription collationTypeSelector

-- | @- setCollationType:@
setCollationType :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> NSFetchIndexElementType -> IO ()
setCollationType nsFetchIndexElementDescription value =
  sendMessage nsFetchIndexElementDescription setCollationTypeSelector value

-- | @- ascending@
ascending :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO Bool
ascending nsFetchIndexElementDescription =
  sendMessage nsFetchIndexElementDescription ascendingSelector

-- | @- setAscending:@
setAscending :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> Bool -> IO ()
setAscending nsFetchIndexElementDescription value =
  sendMessage nsFetchIndexElementDescription setAscendingSelector value

-- | @- indexDescription@
indexDescription :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO (Id NSFetchIndexDescription)
indexDescription nsFetchIndexElementDescription =
  sendMessage nsFetchIndexElementDescription indexDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProperty:collationType:@
initWithProperty_collationTypeSelector :: Selector '[Id NSPropertyDescription, NSFetchIndexElementType] (Id NSFetchIndexElementDescription)
initWithProperty_collationTypeSelector = mkSelector "initWithProperty:collationType:"

-- | @Selector@ for @property@
propertySelector :: Selector '[] (Id NSPropertyDescription)
propertySelector = mkSelector "property"

-- | @Selector@ for @propertyName@
propertyNameSelector :: Selector '[] (Id NSString)
propertyNameSelector = mkSelector "propertyName"

-- | @Selector@ for @collationType@
collationTypeSelector :: Selector '[] NSFetchIndexElementType
collationTypeSelector = mkSelector "collationType"

-- | @Selector@ for @setCollationType:@
setCollationTypeSelector :: Selector '[NSFetchIndexElementType] ()
setCollationTypeSelector = mkSelector "setCollationType:"

-- | @Selector@ for @ascending@
ascendingSelector :: Selector '[] Bool
ascendingSelector = mkSelector "ascending"

-- | @Selector@ for @setAscending:@
setAscendingSelector :: Selector '[Bool] ()
setAscendingSelector = mkSelector "setAscending:"

-- | @Selector@ for @indexDescription@
indexDescriptionSelector :: Selector '[] (Id NSFetchIndexDescription)
indexDescriptionSelector = mkSelector "indexDescription"

