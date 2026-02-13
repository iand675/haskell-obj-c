{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class that represents a machine-readable code's attributes.
--
-- Subclasses encapsulate the formal specification and fields specific to a code type.  Each subclass is sufficient to recreate the unique symbol exactly as seen or used with a custom parser.
--
-- Generated bindings for @CIBarcodeDescriptor@.
module ObjC.CoreImage.CIBarcodeDescriptor
  ( CIBarcodeDescriptor
  , IsCIBarcodeDescriptor(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

