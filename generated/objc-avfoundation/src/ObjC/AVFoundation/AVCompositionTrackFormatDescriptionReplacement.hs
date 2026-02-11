{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCompositionTrackFormatDescriptionReplacement
--
-- A format description and its replacement.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCompositionTrackFormatDescriptionReplacement@.
module ObjC.AVFoundation.AVCompositionTrackFormatDescriptionReplacement
  ( AVCompositionTrackFormatDescriptionReplacement
  , IsAVCompositionTrackFormatDescriptionReplacement(..)
  , originalFormatDescription
  , replacementFormatDescription
  , originalFormatDescriptionSelector
  , replacementFormatDescriptionSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | originalFormatDescription
--
-- The original format description.
--
-- ObjC selector: @- originalFormatDescription@
originalFormatDescription :: IsAVCompositionTrackFormatDescriptionReplacement avCompositionTrackFormatDescriptionReplacement => avCompositionTrackFormatDescriptionReplacement -> IO RawId
originalFormatDescription avCompositionTrackFormatDescriptionReplacement  =
  fmap (RawId . castPtr) $ sendMsg avCompositionTrackFormatDescriptionReplacement (mkSelector "originalFormatDescription") (retPtr retVoid) []

-- | replacementFormatDescription
--
-- The replacement format description.
--
-- ObjC selector: @- replacementFormatDescription@
replacementFormatDescription :: IsAVCompositionTrackFormatDescriptionReplacement avCompositionTrackFormatDescriptionReplacement => avCompositionTrackFormatDescriptionReplacement -> IO RawId
replacementFormatDescription avCompositionTrackFormatDescriptionReplacement  =
  fmap (RawId . castPtr) $ sendMsg avCompositionTrackFormatDescriptionReplacement (mkSelector "replacementFormatDescription") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @originalFormatDescription@
originalFormatDescriptionSelector :: Selector
originalFormatDescriptionSelector = mkSelector "originalFormatDescription"

-- | @Selector@ for @replacementFormatDescription@
replacementFormatDescriptionSelector :: Selector
replacementFormatDescriptionSelector = mkSelector "replacementFormatDescription"

