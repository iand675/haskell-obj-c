{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchDeleteResult@.
module ObjC.CoreData.NSBatchDeleteResult
  ( NSBatchDeleteResult
  , IsNSBatchDeleteResult(..)
  , result
  , resultType
  , resultSelector
  , resultTypeSelector

  -- * Enum types
  , NSBatchDeleteRequestResultType(NSBatchDeleteRequestResultType)
  , pattern NSBatchDeleteResultTypeStatusOnly
  , pattern NSBatchDeleteResultTypeObjectIDs
  , pattern NSBatchDeleteResultTypeCount

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
result :: IsNSBatchDeleteResult nsBatchDeleteResult => nsBatchDeleteResult -> IO RawId
result nsBatchDeleteResult  =
  fmap (RawId . castPtr) $ sendMsg nsBatchDeleteResult (mkSelector "result") (retPtr retVoid) []

-- | @- resultType@
resultType :: IsNSBatchDeleteResult nsBatchDeleteResult => nsBatchDeleteResult -> IO NSBatchDeleteRequestResultType
resultType nsBatchDeleteResult  =
  fmap (coerce :: CULong -> NSBatchDeleteRequestResultType) $ sendMsg nsBatchDeleteResult (mkSelector "resultType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

