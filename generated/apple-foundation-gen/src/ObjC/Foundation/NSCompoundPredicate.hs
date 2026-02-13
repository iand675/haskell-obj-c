{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCompoundPredicate@.
module ObjC.Foundation.NSCompoundPredicate
  ( NSCompoundPredicate
  , IsNSCompoundPredicate(..)
  , initWithType_subpredicates
  , initWithCoder
  , andPredicateWithSubpredicates
  , orPredicateWithSubpredicates
  , notPredicateWithSubpredicate
  , compoundPredicateType
  , subpredicates
  , andPredicateWithSubpredicatesSelector
  , compoundPredicateTypeSelector
  , initWithCoderSelector
  , initWithType_subpredicatesSelector
  , notPredicateWithSubpredicateSelector
  , orPredicateWithSubpredicatesSelector
  , subpredicatesSelector

  -- * Enum types
  , NSCompoundPredicateType(NSCompoundPredicateType)
  , pattern NSNotPredicateType
  , pattern NSAndPredicateType
  , pattern NSOrPredicateType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithType:subpredicates:@
initWithType_subpredicates :: (IsNSCompoundPredicate nsCompoundPredicate, IsNSArray subpredicates) => nsCompoundPredicate -> NSCompoundPredicateType -> subpredicates -> IO (Id NSCompoundPredicate)
initWithType_subpredicates nsCompoundPredicate type_ subpredicates =
  sendOwnedMessage nsCompoundPredicate initWithType_subpredicatesSelector type_ (toNSArray subpredicates)

-- | @- initWithCoder:@
initWithCoder :: (IsNSCompoundPredicate nsCompoundPredicate, IsNSCoder coder) => nsCompoundPredicate -> coder -> IO (Id NSCompoundPredicate)
initWithCoder nsCompoundPredicate coder =
  sendOwnedMessage nsCompoundPredicate initWithCoderSelector (toNSCoder coder)

-- | * Convenience Methods **
--
-- ObjC selector: @+ andPredicateWithSubpredicates:@
andPredicateWithSubpredicates :: IsNSArray subpredicates => subpredicates -> IO (Id NSCompoundPredicate)
andPredicateWithSubpredicates subpredicates =
  do
    cls' <- getRequiredClass "NSCompoundPredicate"
    sendClassMessage cls' andPredicateWithSubpredicatesSelector (toNSArray subpredicates)

-- | @+ orPredicateWithSubpredicates:@
orPredicateWithSubpredicates :: IsNSArray subpredicates => subpredicates -> IO (Id NSCompoundPredicate)
orPredicateWithSubpredicates subpredicates =
  do
    cls' <- getRequiredClass "NSCompoundPredicate"
    sendClassMessage cls' orPredicateWithSubpredicatesSelector (toNSArray subpredicates)

-- | @+ notPredicateWithSubpredicate:@
notPredicateWithSubpredicate :: IsNSPredicate predicate => predicate -> IO (Id NSCompoundPredicate)
notPredicateWithSubpredicate predicate =
  do
    cls' <- getRequiredClass "NSCompoundPredicate"
    sendClassMessage cls' notPredicateWithSubpredicateSelector (toNSPredicate predicate)

-- | @- compoundPredicateType@
compoundPredicateType :: IsNSCompoundPredicate nsCompoundPredicate => nsCompoundPredicate -> IO NSCompoundPredicateType
compoundPredicateType nsCompoundPredicate =
  sendMessage nsCompoundPredicate compoundPredicateTypeSelector

-- | @- subpredicates@
subpredicates :: IsNSCompoundPredicate nsCompoundPredicate => nsCompoundPredicate -> IO (Id NSArray)
subpredicates nsCompoundPredicate =
  sendMessage nsCompoundPredicate subpredicatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:subpredicates:@
initWithType_subpredicatesSelector :: Selector '[NSCompoundPredicateType, Id NSArray] (Id NSCompoundPredicate)
initWithType_subpredicatesSelector = mkSelector "initWithType:subpredicates:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSCompoundPredicate)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @andPredicateWithSubpredicates:@
andPredicateWithSubpredicatesSelector :: Selector '[Id NSArray] (Id NSCompoundPredicate)
andPredicateWithSubpredicatesSelector = mkSelector "andPredicateWithSubpredicates:"

-- | @Selector@ for @orPredicateWithSubpredicates:@
orPredicateWithSubpredicatesSelector :: Selector '[Id NSArray] (Id NSCompoundPredicate)
orPredicateWithSubpredicatesSelector = mkSelector "orPredicateWithSubpredicates:"

-- | @Selector@ for @notPredicateWithSubpredicate:@
notPredicateWithSubpredicateSelector :: Selector '[Id NSPredicate] (Id NSCompoundPredicate)
notPredicateWithSubpredicateSelector = mkSelector "notPredicateWithSubpredicate:"

-- | @Selector@ for @compoundPredicateType@
compoundPredicateTypeSelector :: Selector '[] NSCompoundPredicateType
compoundPredicateTypeSelector = mkSelector "compoundPredicateType"

-- | @Selector@ for @subpredicates@
subpredicatesSelector :: Selector '[] (Id NSArray)
subpredicatesSelector = mkSelector "subpredicates"

