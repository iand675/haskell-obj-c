{-# LANGUAGE PatternSynonyms #-}
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
result :: IsNSBatchInsertResult nsBatchInsertResult => nsBatchInsertResult -> IO RawId
result nsBatchInsertResult  =
  fmap (RawId . castPtr) $ sendMsg nsBatchInsertResult (mkSelector "result") (retPtr retVoid) []

-- | @- resultType@
resultType :: IsNSBatchInsertResult nsBatchInsertResult => nsBatchInsertResult -> IO NSBatchInsertRequestResultType
resultType nsBatchInsertResult  =
  fmap (coerce :: CULong -> NSBatchInsertRequestResultType) $ sendMsg nsBatchInsertResult (mkSelector "resultType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

