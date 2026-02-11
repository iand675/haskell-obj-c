{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a type in a type hierarchy.
--
-- Types may represent files on disk, abstract data types with no on-disk	representation, or even entirely unrelated hierarchical classification	systems such as hardware.
--
-- Older API that does not use @UTType@ typically uses an untyped @NSString@	or @CFStringRef@ to refer to a type by its identifier. To get the	identifier of a type for use with these APIs, use the @identifier@ property	of this class.
--
-- https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/understanding_utis/
--
-- Generated bindings for @UTType@.
module ObjC.ShazamKit.UTType
  ( UTType
  , IsUTType(..)


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

