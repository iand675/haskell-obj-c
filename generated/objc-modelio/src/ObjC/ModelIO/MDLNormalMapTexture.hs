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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initByGeneratingNormalMapWithTexture:name:smoothness:contrast:@
initByGeneratingNormalMapWithTexture_name_smoothness_contrast :: (IsMDLNormalMapTexture mdlNormalMapTexture, IsMDLTexture sourceTexture, IsNSString name) => mdlNormalMapTexture -> sourceTexture -> name -> CFloat -> CFloat -> IO (Id MDLNormalMapTexture)
initByGeneratingNormalMapWithTexture_name_smoothness_contrast mdlNormalMapTexture  sourceTexture name smoothness contrast =
withObjCPtr sourceTexture $ \raw_sourceTexture ->
  withObjCPtr name $ \raw_name ->
      sendMsg mdlNormalMapTexture (mkSelector "initByGeneratingNormalMapWithTexture:name:smoothness:contrast:") (retPtr retVoid) [argPtr (castPtr raw_sourceTexture :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCFloat (fromIntegral smoothness), argCFloat (fromIntegral contrast)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initByGeneratingNormalMapWithTexture:name:smoothness:contrast:@
initByGeneratingNormalMapWithTexture_name_smoothness_contrastSelector :: Selector
initByGeneratingNormalMapWithTexture_name_smoothness_contrastSelector = mkSelector "initByGeneratingNormalMapWithTexture:name:smoothness:contrast:"

