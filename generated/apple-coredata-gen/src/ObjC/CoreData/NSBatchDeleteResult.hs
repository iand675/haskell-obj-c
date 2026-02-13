{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
result :: IsNSBatchDeleteResult nsBatchDeleteResult => nsBatchDeleteResult -> IO RawId
result nsBatchDeleteResult =
  sendMessage nsBatchDeleteResult resultSelector

-- | @- resultType@
resultType :: IsNSBatchDeleteResult nsBatchDeleteResult => nsBatchDeleteResult -> IO NSBatchDeleteRequestResultType
resultType nsBatchDeleteResult =
  sendMessage nsBatchDeleteResult resultTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @result@
resultSelector :: Selector '[] RawId
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSBatchDeleteRequestResultType
resultTypeSelector = mkSelector "resultType"

