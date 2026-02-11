{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration parameters for the download task.
--
-- Download configuration consists of primary and auxiliary content configurations. Primary content configuration represents the primary set of renditions essential for offline playback. Auxiliary content configurations represent additional configurations to complement the primary. For example, the primary content configuration may represent stereo audio renditions and auxiliary configuration may represent complementing multichannel audio renditions.
--
-- It is important to configure your download configuration object appropriately before using it to create a download task. Download task makes a copy of the configuration settings you provide and use those settings to configure the task. Once configured, the task object ignores any changes you make to the NSURLSessionConfiguration object. If you need to modify your settings, you must update the download configuration object and use it to create a new download task object.
--
-- Generated bindings for @AVAssetDownloadConfiguration@.
module ObjC.AVFoundation.AVAssetDownloadConfiguration
  ( AVAssetDownloadConfiguration
  , IsAVAssetDownloadConfiguration(..)
  , init_
  , new
  , downloadConfigurationWithAsset_title
  , setInterstitialMediaSelectionCriteria_forMediaCharacteristic
  , artworkData
  , setArtworkData
  , primaryContentConfiguration
  , auxiliaryContentConfigurations
  , setAuxiliaryContentConfigurations
  , optimizesAuxiliaryContentConfigurations
  , setOptimizesAuxiliaryContentConfigurations
  , downloadsInterstitialAssets
  , setDownloadsInterstitialAssets
  , initSelector
  , newSelector
  , downloadConfigurationWithAsset_titleSelector
  , setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector
  , artworkDataSelector
  , setArtworkDataSelector
  , primaryContentConfigurationSelector
  , auxiliaryContentConfigurationsSelector
  , setAuxiliaryContentConfigurationsSelector
  , optimizesAuxiliaryContentConfigurationsSelector
  , setOptimizesAuxiliaryContentConfigurationsSelector
  , downloadsInterstitialAssetsSelector
  , setDownloadsInterstitialAssetsSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id AVAssetDownloadConfiguration)
init_ avAssetDownloadConfiguration  =
  sendMsg avAssetDownloadConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetDownloadConfiguration)
new  =
  do
    cls' <- getRequiredClass "AVAssetDownloadConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates and initializes a download configuration object.
--
-- This method will throw an exception if AVURLAsset has been invalidated.
--
-- - Parameter asset: The asset to create the download configuration for. - Parameter title: A human readable title for this asset, expected to be as suitable as possible for the user's preferred languages. Will show up in the usage pane of the settings app.
--
-- ObjC selector: @+ downloadConfigurationWithAsset:title:@
downloadConfigurationWithAsset_title :: (IsAVURLAsset asset, IsNSString title) => asset -> title -> IO (Id AVAssetDownloadConfiguration)
downloadConfigurationWithAsset_title asset title =
  do
    cls' <- getRequiredClass "AVAssetDownloadConfiguration"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "downloadConfigurationWithAsset:title:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | Sets media selection on interstitials for this asset
--
-- Typically, interstitial assets have not been discovered when the main download is initiated. This method allows the user to specify AVMediaSelectionCriteria for all interstitials that are discovered. Each AVPlayerMediaSelectionCriteria in the array of criteria specfies a set of criteria for a variant to download.
--
-- - Parameter criteria: The array of selection criteria to set - Parameter mediaCharacteristic: The AVMediaCharacteristic to which the criteria will be applied
--
-- ObjC selector: @- setInterstitialMediaSelectionCriteria:forMediaCharacteristic:@
setInterstitialMediaSelectionCriteria_forMediaCharacteristic :: (IsAVAssetDownloadConfiguration avAssetDownloadConfiguration, IsNSArray criteria, IsNSString mediaCharacteristic) => avAssetDownloadConfiguration -> criteria -> mediaCharacteristic -> IO ()
setInterstitialMediaSelectionCriteria_forMediaCharacteristic avAssetDownloadConfiguration  criteria mediaCharacteristic =
withObjCPtr criteria $ \raw_criteria ->
  withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
      sendMsg avAssetDownloadConfiguration (mkSelector "setInterstitialMediaSelectionCriteria:forMediaCharacteristic:") retVoid [argPtr (castPtr raw_criteria :: Ptr ()), argPtr (castPtr raw_mediaCharacteristic :: Ptr ())]

-- | NSData representing artwork data for this asset. Optional. May be displayed, for example, by the usage pane of the Settings app. Must work with +[UIImage imageWithData:].
--
-- ObjC selector: @- artworkData@
artworkData :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id NSData)
artworkData avAssetDownloadConfiguration  =
  sendMsg avAssetDownloadConfiguration (mkSelector "artworkData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | NSData representing artwork data for this asset. Optional. May be displayed, for example, by the usage pane of the Settings app. Must work with +[UIImage imageWithData:].
--
-- ObjC selector: @- setArtworkData:@
setArtworkData :: (IsAVAssetDownloadConfiguration avAssetDownloadConfiguration, IsNSData value) => avAssetDownloadConfiguration -> value -> IO ()
setArtworkData avAssetDownloadConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetDownloadConfiguration (mkSelector "setArtworkData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The primary content for the download.
--
-- ObjC selector: @- primaryContentConfiguration@
primaryContentConfiguration :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id AVAssetDownloadContentConfiguration)
primaryContentConfiguration avAssetDownloadConfiguration  =
  sendMsg avAssetDownloadConfiguration (mkSelector "primaryContentConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The auxiliary content for the download. Optional.
--
-- By default, auxiliaryContentConfigurations will have one or more default auxiliary content configurations. These content configurations can be augmented with additional content configurations or removed entirely if no auxiliary content is desired.
--
-- ObjC selector: @- auxiliaryContentConfigurations@
auxiliaryContentConfigurations :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id NSArray)
auxiliaryContentConfigurations avAssetDownloadConfiguration  =
  sendMsg avAssetDownloadConfiguration (mkSelector "auxiliaryContentConfigurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The auxiliary content for the download. Optional.
--
-- By default, auxiliaryContentConfigurations will have one or more default auxiliary content configurations. These content configurations can be augmented with additional content configurations or removed entirely if no auxiliary content is desired.
--
-- ObjC selector: @- setAuxiliaryContentConfigurations:@
setAuxiliaryContentConfigurations :: (IsAVAssetDownloadConfiguration avAssetDownloadConfiguration, IsNSArray value) => avAssetDownloadConfiguration -> value -> IO ()
setAuxiliaryContentConfigurations avAssetDownloadConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetDownloadConfiguration (mkSelector "setAuxiliaryContentConfigurations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optimizes auxiliary content selection depending on the primary to minimize total number of video renditions downloaded. True by default.
--
-- For example, if the primary content configuration represents stereo renditions and auxiliary content configuration represents multichannel audio renditions, auxiliary multichannel variant will be chosen so as to avoid downloading duplicate video renditions.
--
-- ObjC selector: @- optimizesAuxiliaryContentConfigurations@
optimizesAuxiliaryContentConfigurations :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO Bool
optimizesAuxiliaryContentConfigurations avAssetDownloadConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetDownloadConfiguration (mkSelector "optimizesAuxiliaryContentConfigurations") retCULong []

-- | Optimizes auxiliary content selection depending on the primary to minimize total number of video renditions downloaded. True by default.
--
-- For example, if the primary content configuration represents stereo renditions and auxiliary content configuration represents multichannel audio renditions, auxiliary multichannel variant will be chosen so as to avoid downloading duplicate video renditions.
--
-- ObjC selector: @- setOptimizesAuxiliaryContentConfigurations:@
setOptimizesAuxiliaryContentConfigurations :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> Bool -> IO ()
setOptimizesAuxiliaryContentConfigurations avAssetDownloadConfiguration  value =
  sendMsg avAssetDownloadConfiguration (mkSelector "setOptimizesAuxiliaryContentConfigurations:") retVoid [argCULong (if value then 1 else 0)]

-- | Download interstitial assets as listed in the index file. False by default.
--
-- Ordinarily, interstitial assets are skipped when downloading content for later playback. Setting this property to true will cause interstitial assets to be downloaded as well. Playback of the downloaded content can then match the experience of online streaming playback as closely as possible.
--
-- ObjC selector: @- downloadsInterstitialAssets@
downloadsInterstitialAssets :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO Bool
downloadsInterstitialAssets avAssetDownloadConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetDownloadConfiguration (mkSelector "downloadsInterstitialAssets") retCULong []

-- | Download interstitial assets as listed in the index file. False by default.
--
-- Ordinarily, interstitial assets are skipped when downloading content for later playback. Setting this property to true will cause interstitial assets to be downloaded as well. Playback of the downloaded content can then match the experience of online streaming playback as closely as possible.
--
-- ObjC selector: @- setDownloadsInterstitialAssets:@
setDownloadsInterstitialAssets :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> Bool -> IO ()
setDownloadsInterstitialAssets avAssetDownloadConfiguration  value =
  sendMsg avAssetDownloadConfiguration (mkSelector "setDownloadsInterstitialAssets:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @downloadConfigurationWithAsset:title:@
downloadConfigurationWithAsset_titleSelector :: Selector
downloadConfigurationWithAsset_titleSelector = mkSelector "downloadConfigurationWithAsset:title:"

-- | @Selector@ for @setInterstitialMediaSelectionCriteria:forMediaCharacteristic:@
setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector :: Selector
setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector = mkSelector "setInterstitialMediaSelectionCriteria:forMediaCharacteristic:"

-- | @Selector@ for @artworkData@
artworkDataSelector :: Selector
artworkDataSelector = mkSelector "artworkData"

-- | @Selector@ for @setArtworkData:@
setArtworkDataSelector :: Selector
setArtworkDataSelector = mkSelector "setArtworkData:"

-- | @Selector@ for @primaryContentConfiguration@
primaryContentConfigurationSelector :: Selector
primaryContentConfigurationSelector = mkSelector "primaryContentConfiguration"

-- | @Selector@ for @auxiliaryContentConfigurations@
auxiliaryContentConfigurationsSelector :: Selector
auxiliaryContentConfigurationsSelector = mkSelector "auxiliaryContentConfigurations"

-- | @Selector@ for @setAuxiliaryContentConfigurations:@
setAuxiliaryContentConfigurationsSelector :: Selector
setAuxiliaryContentConfigurationsSelector = mkSelector "setAuxiliaryContentConfigurations:"

-- | @Selector@ for @optimizesAuxiliaryContentConfigurations@
optimizesAuxiliaryContentConfigurationsSelector :: Selector
optimizesAuxiliaryContentConfigurationsSelector = mkSelector "optimizesAuxiliaryContentConfigurations"

-- | @Selector@ for @setOptimizesAuxiliaryContentConfigurations:@
setOptimizesAuxiliaryContentConfigurationsSelector :: Selector
setOptimizesAuxiliaryContentConfigurationsSelector = mkSelector "setOptimizesAuxiliaryContentConfigurations:"

-- | @Selector@ for @downloadsInterstitialAssets@
downloadsInterstitialAssetsSelector :: Selector
downloadsInterstitialAssetsSelector = mkSelector "downloadsInterstitialAssets"

-- | @Selector@ for @setDownloadsInterstitialAssets:@
setDownloadsInterstitialAssetsSelector :: Selector
setDownloadsInterstitialAssetsSelector = mkSelector "setDownloadsInterstitialAssets:"

