{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitIlluminance@.
module ObjC.Foundation.NSUnitIlluminance
  ( NSUnitIlluminance
  , IsNSUnitIlluminance(..)
  , lux
  , luxSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ lux@
lux :: IO (Id NSUnitIlluminance)
lux  =
  do
    cls' <- getRequiredClass "NSUnitIlluminance"
    sendClassMessage cls' luxSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lux@
luxSelector :: Selector '[] (Id NSUnitIlluminance)
luxSelector = mkSelector "lux"

