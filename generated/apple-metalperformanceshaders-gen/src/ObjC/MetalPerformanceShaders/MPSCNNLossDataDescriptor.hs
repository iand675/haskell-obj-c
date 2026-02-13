{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bytesPerImageSelector
  , bytesPerRowSelector
  , initSelector
  , layoutSelector
  , setBytesPerImageSelector
  , setBytesPerRowSelector

  -- * Enum types
  , MPSDataLayout(MPSDataLayout)
  , pattern MPSDataLayoutHeightxWidthxFeatureChannels
  , pattern MPSDataLayoutFeatureChannelsxHeightxWidth

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO (Id MPSCNNLossDataDescriptor)
init_ mpscnnLossDataDescriptor =
  sendOwnedMessage mpscnnLossDataDescriptor initSelector

-- | layout
--
-- Data layout of loss data. See MPSImage.h for more information.
--
-- This parameter specifies the layout of loss data.
--
-- ObjC selector: @- layout@
layout :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO MPSDataLayout
layout mpscnnLossDataDescriptor =
  sendMessage mpscnnLossDataDescriptor layoutSelector

-- | bytesPerRow
--
-- Row bytes of loss data.
--
-- This parameter specifies the row bytes of loss data.
--
-- ObjC selector: @- bytesPerRow@
bytesPerRow :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO CULong
bytesPerRow mpscnnLossDataDescriptor =
  sendMessage mpscnnLossDataDescriptor bytesPerRowSelector

-- | bytesPerRow
--
-- Row bytes of loss data.
--
-- This parameter specifies the row bytes of loss data.
--
-- ObjC selector: @- setBytesPerRow:@
setBytesPerRow :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> CULong -> IO ()
setBytesPerRow mpscnnLossDataDescriptor value =
  sendMessage mpscnnLossDataDescriptor setBytesPerRowSelector value

-- | bytesPerImage
--
-- Slice bytes of loss data.
--
-- This parameter specifies the slice bytes of loss data.
--
-- ObjC selector: @- bytesPerImage@
bytesPerImage :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> IO CULong
bytesPerImage mpscnnLossDataDescriptor =
  sendMessage mpscnnLossDataDescriptor bytesPerImageSelector

-- | bytesPerImage
--
-- Slice bytes of loss data.
--
-- This parameter specifies the slice bytes of loss data.
--
-- ObjC selector: @- setBytesPerImage:@
setBytesPerImage :: IsMPSCNNLossDataDescriptor mpscnnLossDataDescriptor => mpscnnLossDataDescriptor -> CULong -> IO ()
setBytesPerImage mpscnnLossDataDescriptor value =
  sendMessage mpscnnLossDataDescriptor setBytesPerImageSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSCNNLossDataDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @layout@
layoutSelector :: Selector '[] MPSDataLayout
layoutSelector = mkSelector "layout"

-- | @Selector@ for @bytesPerRow@
bytesPerRowSelector :: Selector '[] CULong
bytesPerRowSelector = mkSelector "bytesPerRow"

-- | @Selector@ for @setBytesPerRow:@
setBytesPerRowSelector :: Selector '[CULong] ()
setBytesPerRowSelector = mkSelector "setBytesPerRow:"

-- | @Selector@ for @bytesPerImage@
bytesPerImageSelector :: Selector '[] CULong
bytesPerImageSelector = mkSelector "bytesPerImage"

-- | @Selector@ for @setBytesPerImage:@
setBytesPerImageSelector :: Selector '[CULong] ()
setBytesPerImageSelector = mkSelector "setBytesPerImage:"

