{-# LANGUAGE PatternSynonyms #-}
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
  , divisionsSelector
  , setDivisionsSelector
  , color1Selector
  , setColor1Selector
  , color2Selector
  , setColor2Selector

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- divisions@
divisions :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> IO CFloat
divisions mdlCheckerboardTexture  =
  sendMsg mdlCheckerboardTexture (mkSelector "divisions") retCFloat []

-- | @- setDivisions:@
setDivisions :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> CFloat -> IO ()
setDivisions mdlCheckerboardTexture  value =
  sendMsg mdlCheckerboardTexture (mkSelector "setDivisions:") retVoid [argCFloat (fromIntegral value)]

-- | @- color1@
color1 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> IO (Ptr ())
color1 mdlCheckerboardTexture  =
  fmap castPtr $ sendMsg mdlCheckerboardTexture (mkSelector "color1") (retPtr retVoid) []

-- | @- setColor1:@
setColor1 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> Ptr () -> IO ()
setColor1 mdlCheckerboardTexture  value =
  sendMsg mdlCheckerboardTexture (mkSelector "setColor1:") retVoid [argPtr value]

-- | @- color2@
color2 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> IO (Ptr ())
color2 mdlCheckerboardTexture  =
  fmap castPtr $ sendMsg mdlCheckerboardTexture (mkSelector "color2") (retPtr retVoid) []

-- | @- setColor2:@
setColor2 :: IsMDLCheckerboardTexture mdlCheckerboardTexture => mdlCheckerboardTexture -> Ptr () -> IO ()
setColor2 mdlCheckerboardTexture  value =
  sendMsg mdlCheckerboardTexture (mkSelector "setColor2:") retVoid [argPtr value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @divisions@
divisionsSelector :: Selector
divisionsSelector = mkSelector "divisions"

-- | @Selector@ for @setDivisions:@
setDivisionsSelector :: Selector
setDivisionsSelector = mkSelector "setDivisions:"

-- | @Selector@ for @color1@
color1Selector :: Selector
color1Selector = mkSelector "color1"

-- | @Selector@ for @setColor1:@
setColor1Selector :: Selector
setColor1Selector = mkSelector "setColor1:"

-- | @Selector@ for @color2@
color2Selector :: Selector
color2Selector = mkSelector "color2"

-- | @Selector@ for @setColor2:@
setColor2Selector :: Selector
setColor2Selector = mkSelector "setColor2:"

