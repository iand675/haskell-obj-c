{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLCheckerboardTexture A two color checkboard with a certain number of divisions
--
-- the texture will be created if data is referenced, otherwise, this             object is merely a description
--
-- Generated bindings for @MDLCheckerboardTexture@.
module ObjC.ModelIO.MDLCheckerboardTexture
  ( MDLCheckerboardTexture
  , IsMDLCheckerboardTexture(..)
  , divisions
  , setDivisions
  , color1
  , setColor1
  , color2
  , setColor2
  , color1Selector
  , color2Selector
  , divisionsSelector
  , setColor1Selector
  , setColor2Selector
  , setDivisionsSelector

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

-- | @- divisions@
divisions :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> IO CFloat
divisions mdlCheckerboardTexture =
  sendMessage mdlCheckerboardTexture divisionsSelector

-- | @- setDivisions:@
setDivisions :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> CFloat -> IO ()
setDivisions mdlCheckerboardTexture value =
  sendMessage mdlCheckerboardTexture setDivisionsSelector value

-- | @- color1@
color1 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> IO (Ptr ())
color1 mdlCheckerboardTexture =
  sendMessage mdlCheckerboardTexture color1Selector

-- | @- setColor1:@
setColor1 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> Ptr () -> IO ()
setColor1 mdlCheckerboardTexture value =
  sendMessage mdlCheckerboardTexture setColor1Selector value

-- | @- color2@
color2 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> IO (Ptr ())
color2 mdlCheckerboardTexture =
  sendMessage mdlCheckerboardTexture color2Selector

-- | @- setColor2:@
setColor2 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> Ptr () -> IO ()
setColor2 mdlCheckerboardTexture value =
  sendMessage mdlCheckerboardTexture setColor2Selector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @divisions@
divisionsSelector :: Selector '[] CFloat
divisionsSelector = mkSelector "divisions"

-- | @Selector@ for @setDivisions:@
setDivisionsSelector :: Selector '[CFloat] ()
setDivisionsSelector = mkSelector "setDivisions:"

-- | @Selector@ for @color1@
color1Selector :: Selector '[] (Ptr ())
color1Selector = mkSelector "color1"

-- | @Selector@ for @setColor1:@
setColor1Selector :: Selector '[Ptr ()] ()
setColor1Selector = mkSelector "setColor1:"

-- | @Selector@ for @color2@
color2Selector :: Selector '[] (Ptr ())
color2Selector = mkSelector "color2"

-- | @Selector@ for @setColor2:@
setColor2Selector :: Selector '[Ptr ()] ()
setColor2Selector = mkSelector "setColor2:"

