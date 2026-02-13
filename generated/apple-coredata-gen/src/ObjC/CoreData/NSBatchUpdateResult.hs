{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchUpdateResult@.
module ObjC.CoreData.NSBatchUpdateResult
  ( NSBatchUpdateResult
  , IsNSBatchUpdateResult(..)
  , result
  , resultType
  , resultSelector
  , resultTypeSelector

  -- * Enum types
  , NSBatchUpdateRequestResultType(NSBatchUpdateRequestResultType)
  , pattern NSStatusOnlyResultType
  , pattern NSUpdatedObjectIDsResultType
  , pattern NSUpdatedObjectsCountResultType

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

-- | @- result@
result :: IsNSBatchUpdateResult nsBatchUpdateResult => nsBatchUpdateResult -> IO RawId
result nsBatchUpdateResult =
  sendMessage nsBatchUpdateResult resultSelector

-- | @- resultType@
resultType :: IsNSBatchUpdateResult nsBatchUpdateResult => nsBatchUpdateResult -> IO NSBatchUpdateRequestResultType
resultType nsBatchUpdateResult =
  sendMessage nsBatchUpdateResult resultTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector '[] RawId
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSBatchUpdateRequestResultType
resultTypeSelector = mkSelector "resultType"

