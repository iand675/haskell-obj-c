{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSearchableItem@.
module ObjC.CoreSpotlight.CSSearchableItem
  ( CSSearchableItem
  , IsCSSearchableItem(..)
  , initWithUniqueIdentifier_domainIdentifier_attributeSet
  , compareByRank
  , uniqueIdentifier
  , setUniqueIdentifier
  , domainIdentifier
  , setDomainIdentifier
  , expirationDate
  , setExpirationDate
  , attributeSet
  , setAttributeSet
  , isUpdate
  , setIsUpdate
  , updateListenerOptions
  , setUpdateListenerOptions
  , attributeSetSelector
  , compareByRankSelector
  , domainIdentifierSelector
  , expirationDateSelector
  , initWithUniqueIdentifier_domainIdentifier_attributeSetSelector
  , isUpdateSelector
  , setAttributeSetSelector
  , setDomainIdentifierSelector
  , setExpirationDateSelector
  , setIsUpdateSelector
  , setUniqueIdentifierSelector
  , setUpdateListenerOptionsSelector
  , uniqueIdentifierSelector
  , updateListenerOptionsSelector

  -- * Enum types
  , CSSearchableItemUpdateListenerOptions(CSSearchableItemUpdateListenerOptions)
  , pattern CSSearchableItemUpdateListenerOptionDefault
  , pattern CSSearchableItemUpdateListenerOptionSummarization
  , pattern CSSearchableItemUpdateListenerOptionPriority
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithUniqueIdentifier:domainIdentifier:attributeSet:@
initWithUniqueIdentifier_domainIdentifier_attributeSet :: (IsCSSearchableItem csSearchableItem, IsNSString uniqueIdentifier, IsNSString domainIdentifier, IsCSSearchableItemAttributeSet attributeSet) => csSearchableItem -> uniqueIdentifier -> domainIdentifier -> attributeSet -> IO (Id CSSearchableItem)
initWithUniqueIdentifier_domainIdentifier_attributeSet csSearchableItem uniqueIdentifier domainIdentifier attributeSet =
  sendOwnedMessage csSearchableItem initWithUniqueIdentifier_domainIdentifier_attributeSetSelector (toNSString uniqueIdentifier) (toNSString domainIdentifier) (toCSSearchableItemAttributeSet attributeSet)

-- | @- compareByRank:@
compareByRank :: (IsCSSearchableItem csSearchableItem, IsCSSearchableItem other) => csSearchableItem -> other -> IO NSComparisonResult
compareByRank csSearchableItem other =
  sendMessage csSearchableItem compareByRankSelector (toCSSearchableItem other)

-- | @- uniqueIdentifier@
uniqueIdentifier :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id NSString)
uniqueIdentifier csSearchableItem =
  sendMessage csSearchableItem uniqueIdentifierSelector

-- | @- setUniqueIdentifier:@
setUniqueIdentifier :: (IsCSSearchableItem csSearchableItem, IsNSString value) => csSearchableItem -> value -> IO ()
setUniqueIdentifier csSearchableItem value =
  sendMessage csSearchableItem setUniqueIdentifierSelector (toNSString value)

-- | @- domainIdentifier@
domainIdentifier :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id NSString)
domainIdentifier csSearchableItem =
  sendMessage csSearchableItem domainIdentifierSelector

-- | @- setDomainIdentifier:@
setDomainIdentifier :: (IsCSSearchableItem csSearchableItem, IsNSString value) => csSearchableItem -> value -> IO ()
setDomainIdentifier csSearchableItem value =
  sendMessage csSearchableItem setDomainIdentifierSelector (toNSString value)

-- | @- expirationDate@
expirationDate :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id NSDate)
expirationDate csSearchableItem =
  sendMessage csSearchableItem expirationDateSelector

-- | @- setExpirationDate:@
setExpirationDate :: (IsCSSearchableItem csSearchableItem, IsNSDate value) => csSearchableItem -> value -> IO ()
setExpirationDate csSearchableItem value =
  sendMessage csSearchableItem setExpirationDateSelector (toNSDate value)

-- | @- attributeSet@
attributeSet :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id CSSearchableItemAttributeSet)
attributeSet csSearchableItem =
  sendMessage csSearchableItem attributeSetSelector

-- | @- setAttributeSet:@
setAttributeSet :: (IsCSSearchableItem csSearchableItem, IsCSSearchableItemAttributeSet value) => csSearchableItem -> value -> IO ()
setAttributeSet csSearchableItem value =
  sendMessage csSearchableItem setAttributeSetSelector (toCSSearchableItemAttributeSet value)

-- | @- isUpdate@
isUpdate :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO Bool
isUpdate csSearchableItem =
  sendMessage csSearchableItem isUpdateSelector

-- | @- setIsUpdate:@
setIsUpdate :: IsCSSearchableItem csSearchableItem => csSearchableItem -> Bool -> IO ()
setIsUpdate csSearchableItem value =
  sendMessage csSearchableItem setIsUpdateSelector value

-- | @- updateListenerOptions@
updateListenerOptions :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO CSSearchableItemUpdateListenerOptions
updateListenerOptions csSearchableItem =
  sendMessage csSearchableItem updateListenerOptionsSelector

-- | @- setUpdateListenerOptions:@
setUpdateListenerOptions :: IsCSSearchableItem csSearchableItem => csSearchableItem -> CSSearchableItemUpdateListenerOptions -> IO ()
setUpdateListenerOptions csSearchableItem value =
  sendMessage csSearchableItem setUpdateListenerOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUniqueIdentifier:domainIdentifier:attributeSet:@
initWithUniqueIdentifier_domainIdentifier_attributeSetSelector :: Selector '[Id NSString, Id NSString, Id CSSearchableItemAttributeSet] (Id CSSearchableItem)
initWithUniqueIdentifier_domainIdentifier_attributeSetSelector = mkSelector "initWithUniqueIdentifier:domainIdentifier:attributeSet:"

-- | @Selector@ for @compareByRank:@
compareByRankSelector :: Selector '[Id CSSearchableItem] NSComparisonResult
compareByRankSelector = mkSelector "compareByRank:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector '[] (Id NSString)
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @setUniqueIdentifier:@
setUniqueIdentifierSelector :: Selector '[Id NSString] ()
setUniqueIdentifierSelector = mkSelector "setUniqueIdentifier:"

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector '[] (Id NSString)
domainIdentifierSelector = mkSelector "domainIdentifier"

-- | @Selector@ for @setDomainIdentifier:@
setDomainIdentifierSelector :: Selector '[Id NSString] ()
setDomainIdentifierSelector = mkSelector "setDomainIdentifier:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector '[Id NSDate] ()
setExpirationDateSelector = mkSelector "setExpirationDate:"

-- | @Selector@ for @attributeSet@
attributeSetSelector :: Selector '[] (Id CSSearchableItemAttributeSet)
attributeSetSelector = mkSelector "attributeSet"

-- | @Selector@ for @setAttributeSet:@
setAttributeSetSelector :: Selector '[Id CSSearchableItemAttributeSet] ()
setAttributeSetSelector = mkSelector "setAttributeSet:"

-- | @Selector@ for @isUpdate@
isUpdateSelector :: Selector '[] Bool
isUpdateSelector = mkSelector "isUpdate"

-- | @Selector@ for @setIsUpdate:@
setIsUpdateSelector :: Selector '[Bool] ()
setIsUpdateSelector = mkSelector "setIsUpdate:"

-- | @Selector@ for @updateListenerOptions@
updateListenerOptionsSelector :: Selector '[] CSSearchableItemUpdateListenerOptions
updateListenerOptionsSelector = mkSelector "updateListenerOptions"

-- | @Selector@ for @setUpdateListenerOptions:@
setUpdateListenerOptionsSelector :: Selector '[CSSearchableItemUpdateListenerOptions] ()
setUpdateListenerOptionsSelector = mkSelector "setUpdateListenerOptions:"

