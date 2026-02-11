{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAEDRMetadata@.
module ObjC.QuartzCore.CAEDRMetadata
  ( CAEDRMetadata
  , IsCAEDRMetadata(..)
  , new
  , init_
  , hdR10MetadataWithDisplayInfo_contentInfo_opticalOutputScale
  , hdR10MetadataWithMinLuminance_maxLuminance_opticalOutputScale
  , hlgMetadataWithAmbientViewingEnvironment
  , hlgMetadata
  , available
  , newSelector
  , initSelector
  , hdR10MetadataWithDisplayInfo_contentInfo_opticalOutputScaleSelector
  , hdR10MetadataWithMinLuminance_maxLuminance_opticalOutputScaleSelector
  , hlgMetadataWithAmbientViewingEnvironmentSelector
  , hlgMetadataSelector
  , availableSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CAEDRMetadata)
new  =
  do
    cls' <- getRequiredClass "CAEDRMetadata"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCAEDRMetadata caedrMetadata => caedrMetadata -> IO (Id CAEDRMetadata)
init_ caedrMetadata  =
  sendMsg caedrMetadata (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ HDR10MetadataWithDisplayInfo:contentInfo:opticalOutputScale:@
hdR10MetadataWithDisplayInfo_contentInfo_opticalOutputScale :: (IsNSData displayData, IsNSData contentData) => displayData -> contentData -> CFloat -> IO (Id CAEDRMetadata)
hdR10MetadataWithDisplayInfo_contentInfo_opticalOutputScale displayData contentData scale =
  do
    cls' <- getRequiredClass "CAEDRMetadata"
    withObjCPtr displayData $ \raw_displayData ->
      withObjCPtr contentData $ \raw_contentData ->
        sendClassMsg cls' (mkSelector "HDR10MetadataWithDisplayInfo:contentInfo:opticalOutputScale:") (retPtr retVoid) [argPtr (castPtr raw_displayData :: Ptr ()), argPtr (castPtr raw_contentData :: Ptr ()), argCFloat (fromIntegral scale)] >>= retainedObject . castPtr

-- | @+ HDR10MetadataWithMinLuminance:maxLuminance:opticalOutputScale:@
hdR10MetadataWithMinLuminance_maxLuminance_opticalOutputScale :: CFloat -> CFloat -> CFloat -> IO (Id CAEDRMetadata)
hdR10MetadataWithMinLuminance_maxLuminance_opticalOutputScale minNits maxNits scale =
  do
    cls' <- getRequiredClass "CAEDRMetadata"
    sendClassMsg cls' (mkSelector "HDR10MetadataWithMinLuminance:maxLuminance:opticalOutputScale:") (retPtr retVoid) [argCFloat (fromIntegral minNits), argCFloat (fromIntegral maxNits), argCFloat (fromIntegral scale)] >>= retainedObject . castPtr

-- | @+ HLGMetadataWithAmbientViewingEnvironment:@
hlgMetadataWithAmbientViewingEnvironment :: IsNSData data_ => data_ -> IO (Id CAEDRMetadata)
hlgMetadataWithAmbientViewingEnvironment data_ =
  do
    cls' <- getRequiredClass "CAEDRMetadata"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "HLGMetadataWithAmbientViewingEnvironment:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ HLGMetadata@
hlgMetadata :: IO (Id CAEDRMetadata)
hlgMetadata  =
  do
    cls' <- getRequiredClass "CAEDRMetadata"
    sendClassMsg cls' (mkSelector "HLGMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ available@
available :: IO Bool
available  =
  do
    cls' <- getRequiredClass "CAEDRMetadata"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "available") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @HDR10MetadataWithDisplayInfo:contentInfo:opticalOutputScale:@
hdR10MetadataWithDisplayInfo_contentInfo_opticalOutputScaleSelector :: Selector
hdR10MetadataWithDisplayInfo_contentInfo_opticalOutputScaleSelector = mkSelector "HDR10MetadataWithDisplayInfo:contentInfo:opticalOutputScale:"

-- | @Selector@ for @HDR10MetadataWithMinLuminance:maxLuminance:opticalOutputScale:@
hdR10MetadataWithMinLuminance_maxLuminance_opticalOutputScaleSelector :: Selector
hdR10MetadataWithMinLuminance_maxLuminance_opticalOutputScaleSelector = mkSelector "HDR10MetadataWithMinLuminance:maxLuminance:opticalOutputScale:"

-- | @Selector@ for @HLGMetadataWithAmbientViewingEnvironment:@
hlgMetadataWithAmbientViewingEnvironmentSelector :: Selector
hlgMetadataWithAmbientViewingEnvironmentSelector = mkSelector "HLGMetadataWithAmbientViewingEnvironment:"

-- | @Selector@ for @HLGMetadata@
hlgMetadataSelector :: Selector
hlgMetadataSelector = mkSelector "HLGMetadata"

-- | @Selector@ for @available@
availableSelector :: Selector
availableSelector = mkSelector "available"

