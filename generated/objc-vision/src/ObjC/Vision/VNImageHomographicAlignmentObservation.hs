{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNImageHomographicAlignmentObservation
--
-- VNImageAlignmentObservation
--
-- An observation describing the results of performing a homographic image alignment.
--
-- Generated bindings for @VNImageHomographicAlignmentObservation@.
module ObjC.Vision.VNImageHomographicAlignmentObservation
  ( VNImageHomographicAlignmentObservation
  , IsVNImageHomographicAlignmentObservation(..)


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

