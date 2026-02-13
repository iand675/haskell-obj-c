{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedPoint3D
--
-- An extension of VNPoint3D that associates an identifier to the point.
--
-- It should be noted that VNRecognizedPoint3D is not intended as an overall replacement of simd float 4x4, but is used by observations that recognize labeled points of interest.
--
-- Generated bindings for @VNRecognizedPoint3D@.
module ObjC.Vision.VNRecognizedPoint3D
  ( VNRecognizedPoint3D
  , IsVNRecognizedPoint3D(..)
  , new
  , init_
  , identifier
  , identifierSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNRecognizedPoint3D)
new  =
  do
    cls' <- getRequiredClass "VNRecognizedPoint3D"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNRecognizedPoint3D vnRecognizedPoint3D => vnRecognizedPoint3D -> IO (Id VNRecognizedPoint3D)
init_ vnRecognizedPoint3D =
  sendOwnedMessage vnRecognizedPoint3D initSelector

-- | The is the identifier that provides context as to the kind of point that was recognized.
--
-- The string is defined by the model that recognized the point. Usually these are technical labels that are not localized and not meant to be used directly to be presented to an end user in the UI.
--
-- ObjC selector: @- identifier@
identifier :: IsVNRecognizedPoint3D vnRecognizedPoint3D => vnRecognizedPoint3D -> IO (Id NSString)
identifier vnRecognizedPoint3D =
  sendMessage vnRecognizedPoint3D identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNRecognizedPoint3D)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNRecognizedPoint3D)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

