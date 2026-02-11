{-# LANGUAGE PatternSynonyms #-}
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
result :: IsNSBatchUpdateResult nsBatchUpdateResult => nsBatchUpdateResult -> IO RawId
result nsBatchUpdateResult  =
  fmap (RawId . castPtr) $ sendMsg nsBatchUpdateResult (mkSelector "result") (retPtr retVoid) []

-- | @- resultType@
resultType :: IsNSBatchUpdateResult nsBatchUpdateResult => nsBatchUpdateResult -> IO NSBatchUpdateRequestResultType
resultType nsBatchUpdateResult  =
  fmap (coerce :: CULong -> NSBatchUpdateRequestResultType) $ sendMsg nsBatchUpdateResult (mkSelector "resultType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

