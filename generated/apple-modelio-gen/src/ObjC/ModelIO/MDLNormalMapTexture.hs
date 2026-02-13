{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLNormalMapTexture@.
module ObjC.ModelIO.MDLNormalMapTexture
  ( MDLNormalMapTexture
  , IsMDLNormalMapTexture(..)
  , initByGeneratingNormalMapWithTexture_name_smoothness_contrast
  , initByGeneratingNormalMapWithTexture_name_smoothness_contrastSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initByGeneratingNormalMapWithTexture:name:smoothness:contrast:@
initByGeneratingNormalMapWithTexture_name_smoothness_contrast :: (IsMDLNormalMapTexture mdlNormalMapTexture, IsMDLTexture sourceTexture, IsNSString name) => mdlNormalMapTexture -> sourceTexture -> name -> CFloat -> CFloat -> IO (Id MDLNormalMapTexture)
initByGeneratingNormalMapWithTexture_name_smoothness_contrast mdlNormalMapTexture sourceTexture name smoothness contrast =
  sendOwnedMessage mdlNormalMapTexture initByGeneratingNormalMapWithTexture_name_smoothness_contrastSelector (toMDLTexture sourceTexture) (toNSString name) smoothness contrast

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initByGeneratingNormalMapWithTexture:name:smoothness:contrast:@
initByGeneratingNormalMapWithTexture_name_smoothness_contrastSelector :: Selector '[Id MDLTexture, Id NSString, CFloat, CFloat] (Id MDLNormalMapTexture)
initByGeneratingNormalMapWithTexture_name_smoothness_contrastSelector = mkSelector "initByGeneratingNormalMapWithTexture:name:smoothness:contrast:"

