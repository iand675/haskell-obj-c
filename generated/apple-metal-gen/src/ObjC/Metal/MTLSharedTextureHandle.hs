{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLSharedTextureHandle@.
module ObjC.Metal.MTLSharedTextureHandle
  ( MTLSharedTextureHandle
  , IsMTLSharedTextureHandle(..)
  , device
  , label
  , deviceSelector
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

-- | device
--
-- The device this texture was created against.
--
-- This shared texture handle can only be used with this device.
--
-- ObjC selector: @- device@
device :: IsMTLSharedTextureHandle mtlSharedTextureHandle => mtlSharedTextureHandle -> IO RawId
device mtlSharedTextureHandle  =
    fmap (RawId . castPtr) $ sendMsg mtlSharedTextureHandle (mkSelector "device") (retPtr retVoid) []

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

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

