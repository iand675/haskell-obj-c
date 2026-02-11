{-# LANGUAGE PatternSynonyms #-}
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
  , initWithType_subpredicatesSelector
  , initWithCoderSelector
  , andPredicateWithSubpredicatesSelector
  , orPredicateWithSubpredicatesSelector
  , notPredicateWithSubpredicateSelector
  , compoundPredicateTypeSelector
  , subpredicatesSelector

  -- * Enum types
  , NSCompoundPredicateType(NSCompoundPredicateType)
  , pattern NSNotPredicateType
  , pattern NSAndPredicateType
  , pattern NSOrPredicateType

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
import ObjC.Foundation.Internal.Enums

-- | @- initWithType:subpredicates:@
initWithType_subpredicates :: (IsNSCompoundPredicate nsCompoundPredicate, IsNSArray subpredicates) => nsCompoundPredicate -> NSCompoundPredicateType -> subpredicates -> IO (Id NSCompoundPredicate)
initWithType_subpredicates nsCompoundPredicate  type_ subpredicates =
withObjCPtr subpredicates $ \raw_subpredicates ->
    sendMsg nsCompoundPredicate (mkSelector "initWithType:subpredicates:") (retPtr retVoid) [argCULong (coerce type_), argPtr (castPtr raw_subpredicates :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSCompoundPredicate nsCompoundPredicate, IsNSCoder coder) => nsCompoundPredicate -> coder -> IO (Id NSCompoundPredicate)
initWithCoder nsCompoundPredicate  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsCompoundPredicate (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | * Convenience Methods **
--
-- ObjC selector: @+ andPredicateWithSubpredicates:@
andPredicateWithSubpredicates :: IsNSArray subpredicates => subpredicates -> IO (Id NSCompoundPredicate)
andPredicateWithSubpredicates subpredicates =
  do
    cls' <- getRequiredClass "NSCompoundPredicate"
    withObjCPtr subpredicates $ \raw_subpredicates ->
      sendClassMsg cls' (mkSelector "andPredicateWithSubpredicates:") (retPtr retVoid) [argPtr (castPtr raw_subpredicates :: Ptr ())] >>= retainedObject . castPtr

-- | @+ orPredicateWithSubpredicates:@
orPredicateWithSubpredicates :: IsNSArray subpredicates => subpredicates -> IO (Id NSCompoundPredicate)
orPredicateWithSubpredicates subpredicates =
  do
    cls' <- getRequiredClass "NSCompoundPredicate"
    withObjCPtr subpredicates $ \raw_subpredicates ->
      sendClassMsg cls' (mkSelector "orPredicateWithSubpredicates:") (retPtr retVoid) [argPtr (castPtr raw_subpredicates :: Ptr ())] >>= retainedObject . castPtr

-- | @+ notPredicateWithSubpredicate:@
notPredicateWithSubpredicate :: IsNSPredicate predicate => predicate -> IO (Id NSCompoundPredicate)
notPredicateWithSubpredicate predicate =
  do
    cls' <- getRequiredClass "NSCompoundPredicate"
    withObjCPtr predicate $ \raw_predicate ->
      sendClassMsg cls' (mkSelector "notPredicateWithSubpredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @- compoundPredicateType@
compoundPredicateType :: IsNSCompoundPredicate nsCompoundPredicate => nsCompoundPredicate -> IO NSCompoundPredicateType
compoundPredicateType nsCompoundPredicate  =
  fmap (coerce :: CULong -> NSCompoundPredicateType) $ sendMsg nsCompoundPredicate (mkSelector "compoundPredicateType") retCULong []

-- | @- subpredicates@
subpredicates :: IsNSCompoundPredicate nsCompoundPredicate => nsCompoundPredicate -> IO (Id NSArray)
subpredicates nsCompoundPredicate  =
  sendMsg nsCompoundPredicate (mkSelector "subpredicates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:subpredicates:@
initWithType_subpredicatesSelector :: Selector
initWithType_subpredicatesSelector = mkSelector "initWithType:subpredicates:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @andPredicateWithSubpredicates:@
andPredicateWithSubpredicatesSelector :: Selector
andPredicateWithSubpredicatesSelector = mkSelector "andPredicateWithSubpredicates:"

-- | @Selector@ for @orPredicateWithSubpredicates:@
orPredicateWithSubpredicatesSelector :: Selector
orPredicateWithSubpredicatesSelector = mkSelector "orPredicateWithSubpredicates:"

-- | @Selector@ for @notPredicateWithSubpredicate:@
notPredicateWithSubpredicateSelector :: Selector
notPredicateWithSubpredicateSelector = mkSelector "notPredicateWithSubpredicate:"

-- | @Selector@ for @compoundPredicateType@
compoundPredicateTypeSelector :: Selector
compoundPredicateTypeSelector = mkSelector "compoundPredicateType"

-- | @Selector@ for @subpredicates@
subpredicatesSelector :: Selector
subpredicatesSelector = mkSelector "subpredicates"

