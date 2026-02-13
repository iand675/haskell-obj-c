{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNull@.
module ObjC.Foundation.NSNull
  ( NSNull
  , IsNSNull(..)
  , null_
  , nullSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ null@
null_ :: IO (Id NSNull)
null_  =
  do
    cls' <- getRequiredClass "NSNull"
    sendClassMessage cls' nullSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @null@
nullSelector :: Selector '[] (Id NSNull)
nullSelector = mkSelector "null"

