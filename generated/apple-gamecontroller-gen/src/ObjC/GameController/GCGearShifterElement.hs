{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A @GCGearShifterElement@ object represents an attached gear shifter.  Both pattern and sequential gear shifters are supported.
--
-- Generated bindings for @GCGearShifterElement@.
module ObjC.GameController.GCGearShifterElement
  ( GCGearShifterElement
  , IsGCGearShifterElement(..)
  , patternInput
  , sequentialInput
  , patternInputSelector
  , sequentialInputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Get the input reporting the position of the pattern gear shifter.  If this property is @nil,@ the gear shifter is not a pattern gear shifter.
--
-- In the returned input, a position of @-1@ corresponds to the "reverse gear". A position of @0@ corresponds to the neutral gear.
--
-- ObjC selector: @- patternInput@
patternInput :: IsGCGearShifterElement gcGearShifterElement => gcGearShifterElement -> IO RawId
patternInput gcGearShifterElement =
  sendMessage gcGearShifterElement patternInputSelector

-- | Get the input reporting changes to the sequential gear shifter.  If this property is @nil,@ the gear shifter is not a sequential gear shifter.
--
-- ObjC selector: @- sequentialInput@
sequentialInput :: IsGCGearShifterElement gcGearShifterElement => gcGearShifterElement -> IO RawId
sequentialInput gcGearShifterElement =
  sendMessage gcGearShifterElement sequentialInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @patternInput@
patternInputSelector :: Selector '[] RawId
patternInputSelector = mkSelector "patternInput"

-- | @Selector@ for @sequentialInput@
sequentialInputSelector :: Selector '[] RawId
sequentialInputSelector = mkSelector "sequentialInput"

