{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchInsertResult@.
module ObjC.CoreData.NSBatchInsertResult
  ( NSBatchInsertResult
  , IsNSBatchInsertResult(..)
  , result
  , resultType
  , resultSelector
  , resultTypeSelector

  -- * Enum types
  , NSBatchInsertRequestResultType(NSBatchInsertRequestResultType)
  , pattern NSBatchInsertRequestResultTypeStatusOnly
  , pattern NSBatchInsertRequestResultTypeObjectIDs
  , pattern NSBatchInsertRequestResultTypeCount

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
result :: IsNSBatchInsertResult nsBatchInsertResult => nsBatchInsertResult -> IO RawId
result nsBatchInsertResult =
  sendMessage nsBatchInsertResult resultSelector

-- | @- resultType@
resultType :: IsNSBatchInsertResult nsBatchInsertResult => nsBatchInsertResult -> IO NSBatchInsertRequestResultType
resultType nsBatchInsertResult =
  sendMessage nsBatchInsertResult resultTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector '[] RawId
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSBatchInsertRequestResultType
resultTypeSelector = mkSelector "resultType"

