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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Get the input reporting the position of the pattern gear shifter.  If this property is @nil,@ the gear shifter is not a pattern gear shifter.
--
-- In the returned input, a position of @-1@ corresponds to the "reverse gear". A position of @0@ corresponds to the neutral gear.
--
-- ObjC selector: @- patternInput@
patternInput :: IsGCGearShifterElement gcGearShifterElement => gcGearShifterElement -> IO RawId
patternInput gcGearShifterElement  =
    fmap (RawId . castPtr) $ sendMsg gcGearShifterElement (mkSelector "patternInput") (retPtr retVoid) []

-- | Get the input reporting changes to the sequential gear shifter.  If this property is @nil,@ the gear shifter is not a sequential gear shifter.
--
-- ObjC selector: @- sequentialInput@
sequentialInput :: IsGCGearShifterElement gcGearShifterElement => gcGearShifterElement -> IO RawId
sequentialInput gcGearShifterElement  =
    fmap (RawId . castPtr) $ sendMsg gcGearShifterElement (mkSelector "sequentialInput") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @patternInput@
patternInputSelector :: Selector
patternInputSelector = mkSelector "patternInput"

-- | @Selector@ for @sequentialInput@
sequentialInputSelector :: Selector
sequentialInputSelector = mkSelector "sequentialInput"

