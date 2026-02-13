{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitDispersion@.
module ObjC.Foundation.NSUnitDispersion
  ( NSUnitDispersion
  , IsNSUnitDispersion(..)
  , partsPerMillion
  , partsPerMillionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ partsPerMillion@
partsPerMillion :: IO (Id NSUnitDispersion)
partsPerMillion  =
  do
    cls' <- getRequiredClass "NSUnitDispersion"
    sendClassMessage cls' partsPerMillionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @partsPerMillion@
partsPerMillionSelector :: Selector '[] (Id NSUnitDispersion)
partsPerMillionSelector = mkSelector "partsPerMillion"

