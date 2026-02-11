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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

