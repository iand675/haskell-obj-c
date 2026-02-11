{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The data model value representing a stroke in a @PKDrawing@.
--
-- Generated bindings for @PKStroke@.
module ObjC.PencilKit.PKStroke
  ( PKStroke
  , IsPKStroke(..)
  , ink
  , path
  , mask
  , maskedPathRanges
  , randomSeed
  , requiredContentVersion
  , inkSelector
  , pathSelector
  , maskSelector
  , maskedPathRangesSelector
  , randomSeedSelector
  , requiredContentVersionSelector

  -- * Enum types
  , PKContentVersion(PKContentVersion)
  , pattern PKContentVersion1
  , pattern PKContentVersion2
  , pattern PKContentVersion3
  , pattern PKContentVersion4
  , pattern PKContentVersionLatest

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

import ObjC.PencilKit.Internal.Classes
import ObjC.PencilKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The ink used to render this stroke.
--
-- ObjC selector: @- ink@
ink :: IsPKStroke pkStroke => pkStroke -> IO (Id PKInk)
ink pkStroke  =
  sendMsg pkStroke (mkSelector "ink") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The B-spline path that describes this stroke.
--
-- ObjC selector: @- path@
path :: IsPKStroke pkStroke => pkStroke -> IO (Id PKStrokePath)
path pkStroke  =
  sendMsg pkStroke (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mask@
mask :: IsPKStroke pkStroke => pkStroke -> IO (Id NSBezierPath)
mask pkStroke  =
  sendMsg pkStroke (mkSelector "mask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | These are the parametric parameter ranges of points in @strokePath@ that intersect the stroke's mask.
--
-- ObjC selector: @- maskedPathRanges@
maskedPathRanges :: IsPKStroke pkStroke => pkStroke -> IO (Id NSArray)
maskedPathRanges pkStroke  =
  sendMsg pkStroke (mkSelector "maskedPathRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The random seed for drawing strokes that use randomized effects.
--
-- ObjC selector: @- randomSeed@
randomSeed :: IsPKStroke pkStroke => pkStroke -> IO CUInt
randomSeed pkStroke  =
  sendMsg pkStroke (mkSelector "randomSeed") retCUInt []

-- | The PencilKit version required to use this stroke.
--
-- ObjC selector: @- requiredContentVersion@
requiredContentVersion :: IsPKStroke pkStroke => pkStroke -> IO PKContentVersion
requiredContentVersion pkStroke  =
  fmap (coerce :: CLong -> PKContentVersion) $ sendMsg pkStroke (mkSelector "requiredContentVersion") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ink@
inkSelector :: Selector
inkSelector = mkSelector "ink"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @mask@
maskSelector :: Selector
maskSelector = mkSelector "mask"

-- | @Selector@ for @maskedPathRanges@
maskedPathRangesSelector :: Selector
maskedPathRangesSelector = mkSelector "maskedPathRanges"

-- | @Selector@ for @randomSeed@
randomSeedSelector :: Selector
randomSeedSelector = mkSelector "randomSeed"

-- | @Selector@ for @requiredContentVersion@
requiredContentVersionSelector :: Selector
requiredContentVersionSelector = mkSelector "requiredContentVersion"

