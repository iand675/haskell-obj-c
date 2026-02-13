{-# LANGUAGE DataKinds #-}
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
  , artworkDataSelector
  , auxiliaryContentConfigurationsSelector
  , downloadConfigurationWithAsset_titleSelector
  , downloadsInterstitialAssetsSelector
  , initSelector
  , newSelector
  , optimizesAuxiliaryContentConfigurationsSelector
  , primaryContentConfigurationSelector
  , setArtworkDataSelector
  , setAuxiliaryContentConfigurationsSelector
  , setDownloadsInterstitialAssetsSelector
  , setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector
  , setOptimizesAuxiliaryContentConfigurationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id AVAssetDownloadConfiguration)
init_ avAssetDownloadConfiguration =
  sendOwnedMessage avAssetDownloadConfiguration initSelector

-- | @+ new@
new :: IO (Id AVAssetDownloadConfiguration)
new  =
  do
    cls' <- getRequiredClass "AVAssetDownloadConfiguration"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' downloadConfigurationWithAsset_titleSelector (toAVURLAsset asset) (toNSString title)

-- | Sets media selection on interstitials for this asset
--
-- Typically, interstitial assets have not been discovered when the main download is initiated. This method allows the user to specify AVMediaSelectionCriteria for all interstitials that are discovered. Each AVPlayerMediaSelectionCriteria in the array of criteria specfies a set of criteria for a variant to download.
--
-- - Parameter criteria: The array of selection criteria to set - Parameter mediaCharacteristic: The AVMediaCharacteristic to which the criteria will be applied
--
-- ObjC selector: @- setInterstitialMediaSelectionCriteria:forMediaCharacteristic:@
setInterstitialMediaSelectionCriteria_forMediaCharacteristic :: (IsAVAssetDownloadConfiguration avAssetDownloadConfiguration, IsNSArray criteria, IsNSString mediaCharacteristic) => avAssetDownloadConfiguration -> criteria -> mediaCharacteristic -> IO ()
setInterstitialMediaSelectionCriteria_forMediaCharacteristic avAssetDownloadConfiguration criteria mediaCharacteristic =
  sendMessage avAssetDownloadConfiguration setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector (toNSArray criteria) (toNSString mediaCharacteristic)

-- | NSData representing artwork data for this asset. Optional. May be displayed, for example, by the usage pane of the Settings app. Must work with +[UIImage imageWithData:].
--
-- ObjC selector: @- artworkData@
artworkData :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id NSData)
artworkData avAssetDownloadConfiguration =
  sendMessage avAssetDownloadConfiguration artworkDataSelector

-- | NSData representing artwork data for this asset. Optional. May be displayed, for example, by the usage pane of the Settings app. Must work with +[UIImage imageWithData:].
--
-- ObjC selector: @- setArtworkData:@
setArtworkData :: (IsAVAssetDownloadConfiguration avAssetDownloadConfiguration, IsNSData value) => avAssetDownloadConfiguration -> value -> IO ()
setArtworkData avAssetDownloadConfiguration value =
  sendMessage avAssetDownloadConfiguration setArtworkDataSelector (toNSData value)

-- | The primary content for the download.
--
-- ObjC selector: @- primaryContentConfiguration@
primaryContentConfiguration :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id AVAssetDownloadContentConfiguration)
primaryContentConfiguration avAssetDownloadConfiguration =
  sendMessage avAssetDownloadConfiguration primaryContentConfigurationSelector

-- | The auxiliary content for the download. Optional.
--
-- By default, auxiliaryContentConfigurations will have one or more default auxiliary content configurations. These content configurations can be augmented with additional content configurations or removed entirely if no auxiliary content is desired.
--
-- ObjC selector: @- auxiliaryContentConfigurations@
auxiliaryContentConfigurations :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO (Id NSArray)
auxiliaryContentConfigurations avAssetDownloadConfiguration =
  sendMessage avAssetDownloadConfiguration auxiliaryContentConfigurationsSelector

-- | The auxiliary content for the download. Optional.
--
-- By default, auxiliaryContentConfigurations will have one or more default auxiliary content configurations. These content configurations can be augmented with additional content configurations or removed entirely if no auxiliary content is desired.
--
-- ObjC selector: @- setAuxiliaryContentConfigurations:@
setAuxiliaryContentConfigurations :: (IsAVAssetDownloadConfiguration avAssetDownloadConfiguration, IsNSArray value) => avAssetDownloadConfiguration -> value -> IO ()
setAuxiliaryContentConfigurations avAssetDownloadConfiguration value =
  sendMessage avAssetDownloadConfiguration setAuxiliaryContentConfigurationsSelector (toNSArray value)

-- | Optimizes auxiliary content selection depending on the primary to minimize total number of video renditions downloaded. True by default.
--
-- For example, if the primary content configuration represents stereo renditions and auxiliary content configuration represents multichannel audio renditions, auxiliary multichannel variant will be chosen so as to avoid downloading duplicate video renditions.
--
-- ObjC selector: @- optimizesAuxiliaryContentConfigurations@
optimizesAuxiliaryContentConfigurations :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO Bool
optimizesAuxiliaryContentConfigurations avAssetDownloadConfiguration =
  sendMessage avAssetDownloadConfiguration optimizesAuxiliaryContentConfigurationsSelector

-- | Optimizes auxiliary content selection depending on the primary to minimize total number of video renditions downloaded. True by default.
--
-- For example, if the primary content configuration represents stereo renditions and auxiliary content configuration represents multichannel audio renditions, auxiliary multichannel variant will be chosen so as to avoid downloading duplicate video renditions.
--
-- ObjC selector: @- setOptimizesAuxiliaryContentConfigurations:@
setOptimizesAuxiliaryContentConfigurations :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> Bool -> IO ()
setOptimizesAuxiliaryContentConfigurations avAssetDownloadConfiguration value =
  sendMessage avAssetDownloadConfiguration setOptimizesAuxiliaryContentConfigurationsSelector value

-- | Download interstitial assets as listed in the index file. False by default.
--
-- Ordinarily, interstitial assets are skipped when downloading content for later playback. Setting this property to true will cause interstitial assets to be downloaded as well. Playback of the downloaded content can then match the experience of online streaming playback as closely as possible.
--
-- ObjC selector: @- downloadsInterstitialAssets@
downloadsInterstitialAssets :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> IO Bool
downloadsInterstitialAssets avAssetDownloadConfiguration =
  sendMessage avAssetDownloadConfiguration downloadsInterstitialAssetsSelector

-- | Download interstitial assets as listed in the index file. False by default.
--
-- Ordinarily, interstitial assets are skipped when downloading content for later playback. Setting this property to true will cause interstitial assets to be downloaded as well. Playback of the downloaded content can then match the experience of online streaming playback as closely as possible.
--
-- ObjC selector: @- setDownloadsInterstitialAssets:@
setDownloadsInterstitialAssets :: IsAVAssetDownloadConfiguration avAssetDownloadConfiguration => avAssetDownloadConfiguration -> Bool -> IO ()
setDownloadsInterstitialAssets avAssetDownloadConfiguration value =
  sendMessage avAssetDownloadConfiguration setDownloadsInterstitialAssetsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetDownloadConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetDownloadConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @downloadConfigurationWithAsset:title:@
downloadConfigurationWithAsset_titleSelector :: Selector '[Id AVURLAsset, Id NSString] (Id AVAssetDownloadConfiguration)
downloadConfigurationWithAsset_titleSelector = mkSelector "downloadConfigurationWithAsset:title:"

-- | @Selector@ for @setInterstitialMediaSelectionCriteria:forMediaCharacteristic:@
setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector :: Selector '[Id NSArray, Id NSString] ()
setInterstitialMediaSelectionCriteria_forMediaCharacteristicSelector = mkSelector "setInterstitialMediaSelectionCriteria:forMediaCharacteristic:"

-- | @Selector@ for @artworkData@
artworkDataSelector :: Selector '[] (Id NSData)
artworkDataSelector = mkSelector "artworkData"

-- | @Selector@ for @setArtworkData:@
setArtworkDataSelector :: Selector '[Id NSData] ()
setArtworkDataSelector = mkSelector "setArtworkData:"

-- | @Selector@ for @primaryContentConfiguration@
primaryContentConfigurationSelector :: Selector '[] (Id AVAssetDownloadContentConfiguration)
primaryContentConfigurationSelector = mkSelector "primaryContentConfiguration"

-- | @Selector@ for @auxiliaryContentConfigurations@
auxiliaryContentConfigurationsSelector :: Selector '[] (Id NSArray)
auxiliaryContentConfigurationsSelector = mkSelector "auxiliaryContentConfigurations"

-- | @Selector@ for @setAuxiliaryContentConfigurations:@
setAuxiliaryContentConfigurationsSelector :: Selector '[Id NSArray] ()
setAuxiliaryContentConfigurationsSelector = mkSelector "setAuxiliaryContentConfigurations:"

-- | @Selector@ for @optimizesAuxiliaryContentConfigurations@
optimizesAuxiliaryContentConfigurationsSelector :: Selector '[] Bool
optimizesAuxiliaryContentConfigurationsSelector = mkSelector "optimizesAuxiliaryContentConfigurations"

-- | @Selector@ for @setOptimizesAuxiliaryContentConfigurations:@
setOptimizesAuxiliaryContentConfigurationsSelector :: Selector '[Bool] ()
setOptimizesAuxiliaryContentConfigurationsSelector = mkSelector "setOptimizesAuxiliaryContentConfigurations:"

-- | @Selector@ for @downloadsInterstitialAssets@
downloadsInterstitialAssetsSelector :: Selector '[] Bool
downloadsInterstitialAssetsSelector = mkSelector "downloadsInterstitialAssets"

-- | @Selector@ for @setDownloadsInterstitialAssets:@
setDownloadsInterstitialAssetsSelector :: Selector '[Bool] ()
setDownloadsInterstitialAssetsSelector = mkSelector "setDownloadsInterstitialAssets:"

