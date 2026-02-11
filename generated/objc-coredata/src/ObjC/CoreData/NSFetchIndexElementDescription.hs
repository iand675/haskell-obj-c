{-# LANGUAGE PatternSynonyms #-}
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
  , initWithProperty_collationTypeSelector
  , propertySelector
  , propertyNameSelector
  , collationTypeSelector
  , setCollationTypeSelector
  , ascendingSelector
  , setAscendingSelector
  , indexDescriptionSelector

  -- * Enum types
  , NSFetchIndexElementType(NSFetchIndexElementType)
  , pattern NSFetchIndexElementTypeBinary
  , pattern NSFetchIndexElementTypeRTree

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

-- | @- initWithProperty:collationType:@
initWithProperty_collationType :: (IsNSFetchIndexElementDescription nsFetchIndexElementDescription, IsNSPropertyDescription property) => nsFetchIndexElementDescription -> property -> NSFetchIndexElementType -> IO (Id NSFetchIndexElementDescription)
initWithProperty_collationType nsFetchIndexElementDescription  property collationType =
withObjCPtr property $ \raw_property ->
    sendMsg nsFetchIndexElementDescription (mkSelector "initWithProperty:collationType:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ()), argCULong (coerce collationType)] >>= ownedObject . castPtr

-- | @- property@
property :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO (Id NSPropertyDescription)
property nsFetchIndexElementDescription  =
  sendMsg nsFetchIndexElementDescription (mkSelector "property") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- propertyName@
propertyName :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO (Id NSString)
propertyName nsFetchIndexElementDescription  =
  sendMsg nsFetchIndexElementDescription (mkSelector "propertyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collationType@
collationType :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO NSFetchIndexElementType
collationType nsFetchIndexElementDescription  =
  fmap (coerce :: CULong -> NSFetchIndexElementType) $ sendMsg nsFetchIndexElementDescription (mkSelector "collationType") retCULong []

-- | @- setCollationType:@
setCollationType :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> NSFetchIndexElementType -> IO ()
setCollationType nsFetchIndexElementDescription  value =
  sendMsg nsFetchIndexElementDescription (mkSelector "setCollationType:") retVoid [argCULong (coerce value)]

-- | @- ascending@
ascending :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO Bool
ascending nsFetchIndexElementDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchIndexElementDescription (mkSelector "ascending") retCULong []

-- | @- setAscending:@
setAscending :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> Bool -> IO ()
setAscending nsFetchIndexElementDescription  value =
  sendMsg nsFetchIndexElementDescription (mkSelector "setAscending:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indexDescription@
indexDescription :: IsNSFetchIndexElementDescription nsFetchIndexElementDescription => nsFetchIndexElementDescription -> IO (Id NSFetchIndexDescription)
indexDescription nsFetchIndexElementDescription  =
  sendMsg nsFetchIndexElementDescription (mkSelector "indexDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProperty:collationType:@
initWithProperty_collationTypeSelector :: Selector
initWithProperty_collationTypeSelector = mkSelector "initWithProperty:collationType:"

-- | @Selector@ for @property@
propertySelector :: Selector
propertySelector = mkSelector "property"

-- | @Selector@ for @propertyName@
propertyNameSelector :: Selector
propertyNameSelector = mkSelector "propertyName"

-- | @Selector@ for @collationType@
collationTypeSelector :: Selector
collationTypeSelector = mkSelector "collationType"

-- | @Selector@ for @setCollationType:@
setCollationTypeSelector :: Selector
setCollationTypeSelector = mkSelector "setCollationType:"

-- | @Selector@ for @ascending@
ascendingSelector :: Selector
ascendingSelector = mkSelector "ascending"

-- | @Selector@ for @setAscending:@
setAscendingSelector :: Selector
setAscendingSelector = mkSelector "setAscending:"

-- | @Selector@ for @indexDescription@
indexDescriptionSelector :: Selector
indexDescriptionSelector = mkSelector "indexDescription"

