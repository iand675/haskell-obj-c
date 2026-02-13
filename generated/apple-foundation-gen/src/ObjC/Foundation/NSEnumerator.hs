{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEnumerator@.
module ObjC.Foundation.NSEnumerator
  ( NSEnumerator
  , IsNSEnumerator(..)
  , nextObject
  , allObjects
  , allObjectsSelector
  , nextObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- nextObject@
nextObject :: IsNSEnumerator nsEnumerator => nsEnumerator -> IO RawId
nextObject nsEnumerator =
  sendMessage nsEnumerator nextObjectSelector

-- | @- allObjects@
allObjects :: IsNSEnumerator nsEnumerator => nsEnumerator -> IO (Id NSArray)
allObjects nsEnumerator =
  sendMessage nsEnumerator allObjectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextObject@
nextObjectSelector :: Selector '[] RawId
nextObjectSelector = mkSelector "nextObject"

-- | @Selector@ for @allObjects@
allObjectsSelector :: Selector '[] (Id NSArray)
allObjectsSelector = mkSelector "allObjects"

