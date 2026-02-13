{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
originalFormatDescription avCompositionTrackFormatDescriptionReplacement =
  sendMessage avCompositionTrackFormatDescriptionReplacement originalFormatDescriptionSelector

-- | replacementFormatDescription
--
-- The replacement format description.
--
-- ObjC selector: @- replacementFormatDescription@
replacementFormatDescription :: IsAVCompositionTrackFormatDescriptionReplacement avCompositionTrackFormatDescriptionReplacement => avCompositionTrackFormatDescriptionReplacement -> IO RawId
replacementFormatDescription avCompositionTrackFormatDescriptionReplacement =
  sendMessage avCompositionTrackFormatDescriptionReplacement replacementFormatDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @originalFormatDescription@
originalFormatDescriptionSelector :: Selector '[] RawId
originalFormatDescriptionSelector = mkSelector "originalFormatDescription"

-- | @Selector@ for @replacementFormatDescription@
replacementFormatDescriptionSelector :: Selector '[] RawId
replacementFormatDescriptionSelector = mkSelector "replacementFormatDescription"

