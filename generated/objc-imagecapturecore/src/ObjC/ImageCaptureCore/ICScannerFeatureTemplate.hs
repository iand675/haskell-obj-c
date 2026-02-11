{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFeatureTemplate
--
-- ICScannerFeatureTemplate object is used to define a group of one or more rectangular scan areas that can be used with a scanner functional unit.
--
-- Generated bindings for @ICScannerFeatureTemplate@.
module ObjC.ImageCaptureCore.ICScannerFeatureTemplate
  ( ICScannerFeatureTemplate
  , IsICScannerFeatureTemplate(..)
  , targets
  , targetsSelector


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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- targets@
targets :: IsICScannerFeatureTemplate icScannerFeatureTemplate => icScannerFeatureTemplate -> IO (Id NSArray)
targets icScannerFeatureTemplate  =
  sendMsg icScannerFeatureTemplate (mkSelector "targets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targets@
targetsSelector :: Selector
targetsSelector = mkSelector "targets"

