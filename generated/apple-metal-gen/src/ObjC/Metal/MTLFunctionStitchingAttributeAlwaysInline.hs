{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

