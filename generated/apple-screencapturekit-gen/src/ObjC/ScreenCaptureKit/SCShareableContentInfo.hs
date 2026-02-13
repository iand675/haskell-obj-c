{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , pointPixelScaleSelector
  , styleSelector

  -- * Enum types
  , SCShareableContentStyle(SCShareableContentStyle)
  , pattern SCShareableContentStyleNone
  , pattern SCShareableContentStyleWindow
  , pattern SCShareableContentStyleDisplay
  , pattern SCShareableContentStyleApplication

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | style of stream
--
-- ObjC selector: @- style@
style :: IsSCShareableContentInfo scShareableContentInfo => scShareableContentInfo -> IO SCShareableContentStyle
style scShareableContentInfo =
  sendMessage scShareableContentInfo styleSelector

-- | Pixel to points scaling factor
--
-- ObjC selector: @- pointPixelScale@
pointPixelScale :: IsSCShareableContentInfo scShareableContentInfo => scShareableContentInfo -> IO CFloat
pointPixelScale scShareableContentInfo =
  sendMessage scShareableContentInfo pointPixelScaleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @style@
styleSelector :: Selector '[] SCShareableContentStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @pointPixelScale@
pointPixelScaleSelector :: Selector '[] CFloat
pointPixelScaleSelector = mkSelector "pointPixelScale"

