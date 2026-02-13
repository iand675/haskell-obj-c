{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
device mtlSharedTextureHandle =
  sendMessage mtlSharedTextureHandle deviceSelector

-- | label
--
-- A copy of the original texture's label property, if any
--
-- ObjC selector: @- label@
label :: IsMTLSharedTextureHandle mtlSharedTextureHandle => mtlSharedTextureHandle -> IO (Id NSString)
label mtlSharedTextureHandle =
  sendMessage mtlSharedTextureHandle labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

