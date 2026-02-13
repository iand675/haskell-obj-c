{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentHistoryResult@.
module ObjC.CoreData.NSPersistentHistoryResult
  ( NSPersistentHistoryResult
  , IsNSPersistentHistoryResult(..)
  , result
  , resultType
  , resultSelector
  , resultTypeSelector

  -- * Enum types
  , NSPersistentHistoryResultType(NSPersistentHistoryResultType)
  , pattern NSPersistentHistoryResultTypeStatusOnly
  , pattern NSPersistentHistoryResultTypeObjectIDs
  , pattern NSPersistentHistoryResultTypeCount
  , pattern NSPersistentHistoryResultTypeTransactionsOnly
  , pattern NSPersistentHistoryResultTypeChangesOnly
  , pattern NSPersistentHistoryResultTypeTransactionsAndChanges

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
result :: IsNSPersistentHistoryResult nsPersistentHistoryResult => nsPersistentHistoryResult -> IO RawId
result nsPersistentHistoryResult =
  sendMessage nsPersistentHistoryResult resultSelector

-- | @- resultType@
resultType :: IsNSPersistentHistoryResult nsPersistentHistoryResult => nsPersistentHistoryResult -> IO NSPersistentHistoryResultType
resultType nsPersistentHistoryResult =
  sendMessage nsPersistentHistoryResult resultTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector '[] RawId
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSPersistentHistoryResultType
resultTypeSelector = mkSelector "resultType"

