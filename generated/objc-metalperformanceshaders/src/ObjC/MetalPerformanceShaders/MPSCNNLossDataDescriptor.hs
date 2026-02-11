{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLossDataDescriptor
--
-- This depends on Metal.framework.
--
-- The MPSCNNLossDataDescriptor specifies a loss data descriptor.              The same descriptor can be used to initialize both the              labels and the optional weights data.
--
-- Generated bindings for @MPSCNNLossDataDescriptor@.
module ObjC.MetalPerformanceShaders.MPSCNNLossDataDescriptor
  ( MPSCNNLossDataDescriptor
  , IsMPSCNNLossDataDescriptor(..)
  , init_
  , layout
  , bytesPerRow
  , setBytesPerRow
  , bytesPerImage
  , setBytesPerImage
  , initSelector
  , layoutSelector
  , bytesPerRowSelector
  , setBytesPerRowSelector
  , bytesPerImageSelector
  , setBytesPerImageSelector

  -- * Enum types
  , MPSDataLayout(MPSDataLayout)
  , pattern MPSDataLayoutHeightxWidthxFeatureChannels
  , pattern MPSDataLayoutFeatureChannelsxHeightxWidth

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO (Id MPSCNNLossDataDescriptor)
init_ mpscnnLossDataDescriptor  =
  sendMsg mpscnnLossDataDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | layout
--
-- Data layout of loss data. See MPSImage.h for more information.
--
-- This parameter specifies the layout of loss data.
--
-- ObjC selector: @- layout@
layout :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO MPSDataLayout
layout mpscnnLossDataDescriptor  =
  fmap (coerce :: CULong -> MPSDataLayout) $ sendMsg mpscnnLossDataDescriptor (mkSelector "layout") retCULong []

-- | bytesPerRow
--
-- Row bytes of loss data.
--
-- This parameter specifies the row bytes of loss data.
--
-- ObjC selector: @- bytesPerRow@
bytesPerRow :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO CULong
bytesPerRow mpscnnLossDataDescriptor  =
  sendMsg mpscnnLossDataDescriptor (mkSelector "bytesPerRow") retCULong []

-- | bytesPerRow
--
-- Row bytes of loss data.
--
-- This parameter specifies the row bytes of loss data.
--
-- ObjC selector: @- setBytesPerRow:@
setBytesPerRow :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> CULong -> IO ()
setBytesPerRow mpscnnLossDataDescriptor  value =
  sendMsg mpscnnLossDataDescriptor (mkSelector "setBytesPerRow:") retVoid [argCULong (fromIntegral value)]

-- | bytesPerImage
--
-- Slice bytes of loss data.
--
-- This parameter specifies the slice bytes of loss data.
--
-- ObjC selector: @- bytesPerImage@
bytesPerImage :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO CULong
bytesPerImage mpscnnLossDataDescriptor  =
  sendMsg mpscnnLossDataDescriptor (mkSelector "bytesPerImage") retCULong []

-- | bytesPerImage
--
-- Slice bytes of loss data.
--
-- This parameter specifies the slice bytes of loss data.
--
-- ObjC selector: @- setBytesPerImage:@
setBytesPerImage :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> CULong -> IO ()
setBytesPerImage mpscnnLossDataDescriptor  value =
  sendMsg mpscnnLossDataDescriptor (mkSelector "setBytesPerImage:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @layout@
layoutSelector :: Selector
layoutSelector = mkSelector "layout"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @setBytesPerRow:@
setBytesPerRowSelector :: Selector
setBytesPerRowSelector = mkSelector "setBytesPerRow:"

-- | @Selector@ for @bytesPerImage@
bytesPerImageSelector :: Selector
bytesPerImageSelector = mkSelector "bytesPerImage"

-- | @Selector@ for @setBytesPerImage:@
setBytesPerImageSelector :: Selector
setBytesPerImageSelector = mkSelector "setBytesPerImage:"

