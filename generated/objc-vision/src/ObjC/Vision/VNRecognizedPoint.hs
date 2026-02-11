{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedPoint
--
-- An extension of VNDetectedPoint that associates an identifier to the point.
--
-- It should be noted that VNRecognizedPoint is not intended as an overall replacement of CGPoint, NSPoint or vec2, but is used by observations that recognize labeled points of interest.
--
-- Generated bindings for @VNRecognizedPoint@.
module ObjC.Vision.VNRecognizedPoint
  ( VNRecognizedPoint
  , IsVNRecognizedPoint(..)
  , identifier
  , identifierSelector


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

-- | The is the identifier that provides context as to the kind of point that was recognized.
--
-- The string is defined by the model that recognized the point. Usually these are technical labels that are not localized and not meant to be used directly to be presented to an end user in the UI.
--
-- ObjC selector: @- identifier@
identifier :: IsVNRecognizedPoint vnRecognizedPoint => vnRecognizedPoint -> IO (Id NSString)
identifier vnRecognizedPoint  =
  sendMsg vnRecognizedPoint (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

