{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- targets@
targets :: IsICScannerFeatureTemplate icScannerFeatureTemplate => icScannerFeatureTemplate -> IO (Id NSArray)
targets icScannerFeatureTemplate =
  sendMessage icScannerFeatureTemplate targetsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targets@
targetsSelector :: Selector '[] (Id NSArray)
targetsSelector = mkSelector "targets"

