{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLNoiseTexture  a noise texture containing vector or scalar noise
--
-- the texture will be created if data is referenced, otherwise, this object is merely a description
--
-- Generated bindings for @MDLNoiseTexture@.
module ObjC.ModelIO.MDLNoiseTexture
  ( MDLNoiseTexture
  , IsMDLNoiseTexture(..)

  -- * Enum types
  , MDLTextureChannelEncoding(MDLTextureChannelEncoding)
  , pattern MDLTextureChannelEncodingUInt8
  , pattern MDLTextureChannelEncodingUint8
  , pattern MDLTextureChannelEncodingUInt16
  , pattern MDLTextureChannelEncodingUint16
  , pattern MDLTextureChannelEncodingUInt24
  , pattern MDLTextureChannelEncodingUint24
  , pattern MDLTextureChannelEncodingUInt32
  , pattern MDLTextureChannelEncodingUint32
  , pattern MDLTextureChannelEncodingFloat16
  , pattern MDLTextureChannelEncodingFloat16SR
  , pattern MDLTextureChannelEncodingFloat32

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

