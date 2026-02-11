{-# LANGUAGE PatternSynonyms #-}
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

-- | @- result@
result :: IsNSPersistentHistoryResult nsPersistentHistoryResult => nsPersistentHistoryResult -> IO RawId
result nsPersistentHistoryResult  =
  fmap (RawId . castPtr) $ sendMsg nsPersistentHistoryResult (mkSelector "result") (retPtr retVoid) []

-- | @- resultType@
resultType :: IsNSPersistentHistoryResult nsPersistentHistoryResult => nsPersistentHistoryResult -> IO NSPersistentHistoryResultType
resultType nsPersistentHistoryResult  =
  fmap (coerce :: CLong -> NSPersistentHistoryResultType) $ sendMsg nsPersistentHistoryResult (mkSelector "resultType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

