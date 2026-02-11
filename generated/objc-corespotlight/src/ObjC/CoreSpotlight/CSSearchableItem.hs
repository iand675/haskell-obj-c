{-# LANGUAGE PatternSynonyms #-}
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
  , initWithUniqueIdentifier_domainIdentifier_attributeSetSelector
  , compareByRankSelector
  , uniqueIdentifierSelector
  , setUniqueIdentifierSelector
  , domainIdentifierSelector
  , setDomainIdentifierSelector
  , expirationDateSelector
  , setExpirationDateSelector
  , attributeSetSelector
  , setAttributeSetSelector
  , isUpdateSelector
  , setIsUpdateSelector
  , updateListenerOptionsSelector
  , setUpdateListenerOptionsSelector

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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithUniqueIdentifier:domainIdentifier:attributeSet:@
initWithUniqueIdentifier_domainIdentifier_attributeSet :: (IsCSSearchableItem csSearchableItem, IsNSString uniqueIdentifier, IsNSString domainIdentifier, IsCSSearchableItemAttributeSet attributeSet) => csSearchableItem -> uniqueIdentifier -> domainIdentifier -> attributeSet -> IO (Id CSSearchableItem)
initWithUniqueIdentifier_domainIdentifier_attributeSet csSearchableItem  uniqueIdentifier domainIdentifier attributeSet =
withObjCPtr uniqueIdentifier $ \raw_uniqueIdentifier ->
  withObjCPtr domainIdentifier $ \raw_domainIdentifier ->
    withObjCPtr attributeSet $ \raw_attributeSet ->
        sendMsg csSearchableItem (mkSelector "initWithUniqueIdentifier:domainIdentifier:attributeSet:") (retPtr retVoid) [argPtr (castPtr raw_uniqueIdentifier :: Ptr ()), argPtr (castPtr raw_domainIdentifier :: Ptr ()), argPtr (castPtr raw_attributeSet :: Ptr ())] >>= ownedObject . castPtr

-- | @- compareByRank:@
compareByRank :: (IsCSSearchableItem csSearchableItem, IsCSSearchableItem other) => csSearchableItem -> other -> IO NSComparisonResult
compareByRank csSearchableItem  other =
withObjCPtr other $ \raw_other ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg csSearchableItem (mkSelector "compareByRank:") retCLong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- uniqueIdentifier@
uniqueIdentifier :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id NSString)
uniqueIdentifier csSearchableItem  =
  sendMsg csSearchableItem (mkSelector "uniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniqueIdentifier:@
setUniqueIdentifier :: (IsCSSearchableItem csSearchableItem, IsNSString value) => csSearchableItem -> value -> IO ()
setUniqueIdentifier csSearchableItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItem (mkSelector "setUniqueIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- domainIdentifier@
domainIdentifier :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id NSString)
domainIdentifier csSearchableItem  =
  sendMsg csSearchableItem (mkSelector "domainIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDomainIdentifier:@
setDomainIdentifier :: (IsCSSearchableItem csSearchableItem, IsNSString value) => csSearchableItem -> value -> IO ()
setDomainIdentifier csSearchableItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItem (mkSelector "setDomainIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expirationDate@
expirationDate :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id NSDate)
expirationDate csSearchableItem  =
  sendMsg csSearchableItem (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpirationDate:@
setExpirationDate :: (IsCSSearchableItem csSearchableItem, IsNSDate value) => csSearchableItem -> value -> IO ()
setExpirationDate csSearchableItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItem (mkSelector "setExpirationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributeSet@
attributeSet :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO (Id CSSearchableItemAttributeSet)
attributeSet csSearchableItem  =
  sendMsg csSearchableItem (mkSelector "attributeSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeSet:@
setAttributeSet :: (IsCSSearchableItem csSearchableItem, IsCSSearchableItemAttributeSet value) => csSearchableItem -> value -> IO ()
setAttributeSet csSearchableItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchableItem (mkSelector "setAttributeSet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isUpdate@
isUpdate :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO Bool
isUpdate csSearchableItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg csSearchableItem (mkSelector "isUpdate") retCULong []

-- | @- setIsUpdate:@
setIsUpdate :: IsCSSearchableItem csSearchableItem => csSearchableItem -> Bool -> IO ()
setIsUpdate csSearchableItem  value =
  sendMsg csSearchableItem (mkSelector "setIsUpdate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- updateListenerOptions@
updateListenerOptions :: IsCSSearchableItem csSearchableItem => csSearchableItem -> IO CSSearchableItemUpdateListenerOptions
updateListenerOptions csSearchableItem  =
  fmap (coerce :: CULong -> CSSearchableItemUpdateListenerOptions) $ sendMsg csSearchableItem (mkSelector "updateListenerOptions") retCULong []

-- | @- setUpdateListenerOptions:@
setUpdateListenerOptions :: IsCSSearchableItem csSearchableItem => csSearchableItem -> CSSearchableItemUpdateListenerOptions -> IO ()
setUpdateListenerOptions csSearchableItem  value =
  sendMsg csSearchableItem (mkSelector "setUpdateListenerOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUniqueIdentifier:domainIdentifier:attributeSet:@
initWithUniqueIdentifier_domainIdentifier_attributeSetSelector :: Selector
initWithUniqueIdentifier_domainIdentifier_attributeSetSelector = mkSelector "initWithUniqueIdentifier:domainIdentifier:attributeSet:"

-- | @Selector@ for @compareByRank:@
compareByRankSelector :: Selector
compareByRankSelector = mkSelector "compareByRank:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @setUniqueIdentifier:@
setUniqueIdentifierSelector :: Selector
setUniqueIdentifierSelector = mkSelector "setUniqueIdentifier:"

-- | @Selector@ for @domainIdentifier@
domainIdentifierSelector :: Selector
domainIdentifierSelector = mkSelector "domainIdentifier"

-- | @Selector@ for @setDomainIdentifier:@
setDomainIdentifierSelector :: Selector
setDomainIdentifierSelector = mkSelector "setDomainIdentifier:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector
setExpirationDateSelector = mkSelector "setExpirationDate:"

-- | @Selector@ for @attributeSet@
attributeSetSelector :: Selector
attributeSetSelector = mkSelector "attributeSet"

-- | @Selector@ for @setAttributeSet:@
setAttributeSetSelector :: Selector
setAttributeSetSelector = mkSelector "setAttributeSet:"

-- | @Selector@ for @isUpdate@
isUpdateSelector :: Selector
isUpdateSelector = mkSelector "isUpdate"

-- | @Selector@ for @setIsUpdate:@
setIsUpdateSelector :: Selector
setIsUpdateSelector = mkSelector "setIsUpdate:"

-- | @Selector@ for @updateListenerOptions@
updateListenerOptionsSelector :: Selector
updateListenerOptionsSelector = mkSelector "updateListenerOptions"

-- | @Selector@ for @setUpdateListenerOptions:@
setUpdateListenerOptionsSelector :: Selector
setUpdateListenerOptionsSelector = mkSelector "setUpdateListenerOptions:"

