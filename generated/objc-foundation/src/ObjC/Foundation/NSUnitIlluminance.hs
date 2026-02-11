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

import ObjC.Foundation.Internal.Classes

-- | @+ lux@
lux :: IO (Id NSUnitIlluminance)
lux  =
  do
    cls' <- getRequiredClass "NSUnitIlluminance"
    sendClassMsg cls' (mkSelector "lux") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lux@
luxSelector :: Selector
luxSelector = mkSelector "lux"

