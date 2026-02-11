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

-- | @+ partsPerMillion@
partsPerMillion :: IO (Id NSUnitDispersion)
partsPerMillion  =
  do
    cls' <- getRequiredClass "NSUnitDispersion"
    sendClassMsg cls' (mkSelector "partsPerMillion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @partsPerMillion@
partsPerMillionSelector :: Selector
partsPerMillionSelector = mkSelector "partsPerMillion"

