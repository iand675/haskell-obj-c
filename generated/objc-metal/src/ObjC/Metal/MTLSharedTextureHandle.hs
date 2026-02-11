{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLSharedTextureHandle@.
module ObjC.Metal.MTLSharedTextureHandle
  ( MTLSharedTextureHandle
  , IsMTLSharedTextureHandle(..)
  , label
  , labelSelector


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

-- | label
--
-- A copy of the original texture's label property, if any
--
-- ObjC selector: @- label@
label :: IsMTLSharedTextureHandle mtlSharedTextureHandle => mtlSharedTextureHandle -> IO (Id NSString)
label mtlSharedTextureHandle  =
  sendMsg mtlSharedTextureHandle (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

