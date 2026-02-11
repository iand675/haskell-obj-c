{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCShareableContentInfo
--
-- SCShareableContentInformation is an object that has information about the content of the stream
--
-- Generated bindings for @SCShareableContentInfo@.
module ObjC.ScreenCaptureKit.SCShareableContentInfo
  ( SCShareableContentInfo
  , IsSCShareableContentInfo(..)
  , style
  , pointPixelScale
  , styleSelector
  , pointPixelScaleSelector

  -- * Enum types
  , SCShareableContentStyle(SCShareableContentStyle)
  , pattern SCShareableContentStyleNone
  , pattern SCShareableContentStyleWindow
  , pattern SCShareableContentStyleDisplay
  , pattern SCShareableContentStyleApplication

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | style of stream
--
-- ObjC selector: @- style@
style :: IsSCShareableContentInfo scShareableContentInfo => scShareableContentInfo -> IO SCShareableContentStyle
style scShareableContentInfo  =
  fmap (coerce :: CLong -> SCShareableContentStyle) $ sendMsg scShareableContentInfo (mkSelector "style") retCLong []

-- | Pixel to points scaling factor
--
-- ObjC selector: @- pointPixelScale@
pointPixelScale :: IsSCShareableContentInfo scShareableContentInfo => scShareableContentInfo -> IO CFloat
pointPixelScale scShareableContentInfo  =
  sendMsg scShareableContentInfo (mkSelector "pointPixelScale") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @pointPixelScale@
pointPixelScaleSelector :: Selector
pointPixelScaleSelector = mkSelector "pointPixelScale"

