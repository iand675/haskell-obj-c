{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLFunctionStitchingAttributeAlwaysInline
--
-- Applies the @__attribute__((always_inline))@ attribute to the produced stitched function.
--
-- Generated bindings for @MTLFunctionStitchingAttributeAlwaysInline@.
module ObjC.Metal.MTLFunctionStitchingAttributeAlwaysInline
  ( MTLFunctionStitchingAttributeAlwaysInline
  , IsMTLFunctionStitchingAttributeAlwaysInline(..)


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

