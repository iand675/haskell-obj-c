{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The qualifier of an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetVariantQualifier@.
module ObjC.AVFoundation.AVAssetVariantQualifier
  ( AVAssetVariantQualifier
  , IsAVAssetVariantQualifier(..)
  , init_
  , new
  , assetVariantQualifierWithPredicate
  , assetVariantQualifierWithVariant
  , assetVariantQualifierForMinimumValueInKeyPath
  , assetVariantQualifierForMaximumValueInKeyPath
  , predicateForChannelCount_mediaSelectionOption_operatorType
  , predicateForBinauralAudio_mediaSelectionOption
  , predicateForImmersiveAudio_mediaSelectionOption
  , predicateForDownmixAudio_mediaSelectionOption
  , predicateForPresentationWidth_operatorType
  , predicateForPresentationHeight_operatorType
  , predicateForAudioSampleRate_mediaSelectionOption_operatorType
  , predicateForChannelCount_operatorType
  , predicateForBinauralAudio
  , predicateForImmersiveAudio
  , predicateForDownmixAudio
  , predicateForAudioSampleRate_operatorType
  , assetVariantQualifierForMaximumValueInKeyPathSelector
  , assetVariantQualifierForMinimumValueInKeyPathSelector
  , assetVariantQualifierWithPredicateSelector
  , assetVariantQualifierWithVariantSelector
  , initSelector
  , newSelector
  , predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector
  , predicateForAudioSampleRate_operatorTypeSelector
  , predicateForBinauralAudioSelector
  , predicateForBinauralAudio_mediaSelectionOptionSelector
  , predicateForChannelCount_mediaSelectionOption_operatorTypeSelector
  , predicateForChannelCount_operatorTypeSelector
  , predicateForDownmixAudioSelector
  , predicateForDownmixAudio_mediaSelectionOptionSelector
  , predicateForImmersiveAudioSelector
  , predicateForImmersiveAudio_mediaSelectionOptionSelector
  , predicateForPresentationHeight_operatorTypeSelector
  , predicateForPresentationWidth_operatorTypeSelector

  -- * Enum types
  , NSPredicateOperatorType(NSPredicateOperatorType)
  , pattern NSLessThanPredicateOperatorType
  , pattern NSLessThanOrEqualToPredicateOperatorType
  , pattern NSGreaterThanPredicateOperatorType
  , pattern NSGreaterThanOrEqualToPredicateOperatorType
  , pattern NSEqualToPredicateOperatorType
  , pattern NSNotEqualToPredicateOperatorType
  , pattern NSMatchesPredicateOperatorType
  , pattern NSLikePredicateOperatorType
  , pattern NSBeginsWithPredicateOperatorType
  , pattern NSEndsWithPredicateOperatorType
  , pattern NSInPredicateOperatorType
  , pattern NSCustomSelectorPredicateOperatorType
  , pattern NSContainsPredicateOperatorType
  , pattern NSBetweenPredicateOperatorType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetVariantQualifier avAssetVariantQualifier => avAssetVariantQualifier -> IO (Id AVAssetVariantQualifier)
init_ avAssetVariantQualifier =
  sendOwnedMessage avAssetVariantQualifier initSelector

-- | @+ new@
new :: IO (Id AVAssetVariantQualifier)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendOwnedClassMessage cls' newSelector

-- | Returns a qualifer for a predicate.
--
-- - Parameter predicate: The variant predicate. Must be a valid, non-nil NSPredicate.
--
-- ObjC selector: @+ assetVariantQualifierWithPredicate:@
assetVariantQualifierWithPredicate :: IsNSPredicate predicate => predicate -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierWithPredicate predicate =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' assetVariantQualifierWithPredicateSelector (toNSPredicate predicate)

-- | Returns a qualifer for a particular asset variant.
--
-- - Parameter variant: A variant obtained from the -[AVAsset variants] or -[AVAssetDownloadConfiguration playableVariants]. Must be a valid, non-nil AVAssetVariant.
--
-- ObjC selector: @+ assetVariantQualifierWithVariant:@
assetVariantQualifierWithVariant :: IsAVAssetVariant variant => variant -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierWithVariant variant =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' assetVariantQualifierWithVariantSelector (toAVAssetVariant variant)

-- | Returns a qualifer for finding variant with minimum value in the input key path.
--
-- - Parameter keyPath: AVAssetVariant keyPath. Allowed keyPath values are peakBitRate, averageBitRate, videoAttributes.presentationSize. Must be a valid, non-nil NSString.
--
-- ObjC selector: @+ assetVariantQualifierForMinimumValueInKeyPath:@
assetVariantQualifierForMinimumValueInKeyPath :: IsNSString keyPath => keyPath -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierForMinimumValueInKeyPath keyPath =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' assetVariantQualifierForMinimumValueInKeyPathSelector (toNSString keyPath)

-- | Returns a qualifer for finding variant with maximum value in the input key path
--
-- - Parameter keyPath: AVAssetVariant keyPath. Allowed keyPath values are peakBitRate, averageBitRate, videoAttributes.presentationSize. Must be a valid, non-nil NSString.
--
-- ObjC selector: @+ assetVariantQualifierForMaximumValueInKeyPath:@
assetVariantQualifierForMaximumValueInKeyPath :: IsNSString keyPath => keyPath -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierForMaximumValueInKeyPath keyPath =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' assetVariantQualifierForMaximumValueInKeyPathSelector (toNSString keyPath)

-- | Creates a NSPredicate for audio channel count which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter channelCount: The RHS value for the channel count in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForChannelCount:mediaSelectionOption:operatorType:@
predicateForChannelCount_mediaSelectionOption_operatorType :: IsAVMediaSelectionOption mediaSelectionOption => CLong -> mediaSelectionOption -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForChannelCount_mediaSelectionOption_operatorType channelCount mediaSelectionOption operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForChannelCount_mediaSelectionOption_operatorTypeSelector channelCount (toAVMediaSelectionOption mediaSelectionOption) operatorType

-- | Creates a NSPredicate for binaural which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isBinaural: The RHS value for the value of isBinauralAudio in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration.
--
-- ObjC selector: @+ predicateForBinauralAudio:mediaSelectionOption:@
predicateForBinauralAudio_mediaSelectionOption :: IsAVMediaSelectionOption mediaSelectionOption => Bool -> mediaSelectionOption -> IO (Id NSPredicate)
predicateForBinauralAudio_mediaSelectionOption isBinauralAudio mediaSelectionOption =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForBinauralAudio_mediaSelectionOptionSelector isBinauralAudio (toAVMediaSelectionOption mediaSelectionOption)

-- | Creates a NSPredicate for immersive audio which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isImmersiveAudio: The RHS value for the value of isImmersiveAudio in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration.
--
-- ObjC selector: @+ predicateForImmersiveAudio:mediaSelectionOption:@
predicateForImmersiveAudio_mediaSelectionOption :: IsAVMediaSelectionOption mediaSelectionOption => Bool -> mediaSelectionOption -> IO (Id NSPredicate)
predicateForImmersiveAudio_mediaSelectionOption isImmersiveAudio mediaSelectionOption =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForImmersiveAudio_mediaSelectionOptionSelector isImmersiveAudio (toAVMediaSelectionOption mediaSelectionOption)

-- | Creates a NSPredicate for immersive audio which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isDownmixAudio: The RHS value for the value of isDownmixAudio in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration.
--
-- ObjC selector: @+ predicateForDownmixAudio:mediaSelectionOption:@
predicateForDownmixAudio_mediaSelectionOption :: IsAVMediaSelectionOption mediaSelectionOption => Bool -> mediaSelectionOption -> IO (Id NSPredicate)
predicateForDownmixAudio_mediaSelectionOption isDownmixAudio mediaSelectionOption =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForDownmixAudio_mediaSelectionOptionSelector isDownmixAudio (toAVMediaSelectionOption mediaSelectionOption)

-- | Creates a NSPredicate for presentation size width which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter width: The RHS value for the presentation size width in the predicate equation. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForPresentationWidth:operatorType:@
predicateForPresentationWidth_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForPresentationWidth_operatorType width operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForPresentationWidth_operatorTypeSelector width operatorType

-- | Creates a NSPredicate for presentation size height which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter height: The RHS value for the presentation size height in the predicate equation. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForPresentationHeight:operatorType:@
predicateForPresentationHeight_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForPresentationHeight_operatorType height operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForPresentationHeight_operatorTypeSelector height operatorType

-- | Creates a NSPredicate for audio sample rate which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter sampleRate: The RHS value for the sample rate in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForAudioSampleRate:mediaSelectionOption:operatorType:@
predicateForAudioSampleRate_mediaSelectionOption_operatorType :: IsAVMediaSelectionOption mediaSelectionOption => CDouble -> mediaSelectionOption -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForAudioSampleRate_mediaSelectionOption_operatorType sampleRate mediaSelectionOption operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector sampleRate (toAVMediaSelectionOption mediaSelectionOption) operatorType

-- | Creates a NSPredicate for audio channel count which can be used with other NSPredicates to express variant preferences.
--
-- Predicate will be evaluated on the media selection option selected for the asset. Media selection options for primary assets may be specified in the AVAssetDownloadConfiguration mediaSelections property. Media selection options for interstitial assets may be circumscribed by -[AVAssetDownloadConfiguration setInterstitialMediaSelectionCriteria: forMediaCharacteristic:].
--
-- - Parameter channelCount: The RHS value for the channel count in the predicate equation. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForChannelCount:operatorType:@
predicateForChannelCount_operatorType :: CLong -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForChannelCount_operatorType channelCount operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForChannelCount_operatorTypeSelector channelCount operatorType

-- | Creates a NSPredicate for binaural which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isBinaural: The RHS value for the value of isBinauralAudio in the predicate equation.
--
-- ObjC selector: @+ predicateForBinauralAudio:@
predicateForBinauralAudio :: Bool -> IO (Id NSPredicate)
predicateForBinauralAudio isBinauralAudio =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForBinauralAudioSelector isBinauralAudio

-- | Creates a NSPredicate for immersive audio which can be used with other NSPredicates to express variant preferences.
--
-- Predicate will be evaluated on the media selection option selected for the asset. Media selection options for primary assets may be specified in the AVAssetDownloadConfiguration mediaSelections property. Media selection options for interstitial assets may be circumscribed by -[AVAssetDownloadConfiguration setInterstitialMediaSelectionCriteria: forMediaCharacteristic:].
--
-- - Parameter isImmersiveAudio: The RHS value for the value of isImmersiveAudio in the predicate equation.
--
-- ObjC selector: @+ predicateForImmersiveAudio:@
predicateForImmersiveAudio :: Bool -> IO (Id NSPredicate)
predicateForImmersiveAudio isImmersiveAudio =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForImmersiveAudioSelector isImmersiveAudio

-- | Creates a NSPredicate for immersive audio which can be used with other NSPredicates to express variant preferences.
--
-- Predicate will be evaluated on the media selection option selected for the asset. Media selection options for primary assets may be specified in the AVAssetDownloadConfiguration mediaSelections property. Media selection options for interstitial assets may be circumscribed by -[AVAssetDownloadConfiguration setInterstitialMediaSelectionCriteria: forMediaCharacteristic:].
--
-- - Parameter isDownmixAudio: The RHS value for the value of isDownmixAudio in the predicate equation.
--
-- ObjC selector: @+ predicateForDownmixAudio:@
predicateForDownmixAudio :: Bool -> IO (Id NSPredicate)
predicateForDownmixAudio isDownmixAudio =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForDownmixAudioSelector isDownmixAudio

-- | Creates a NSPredicate for audio sample rate which can be used with other NSPredicates to express variant preferences.
--
-- Predicate will be evaluated on the media selection option selected for the asset. Media selection options for primary assets may be specified in the AVAssetDownloadConfiguration mediaSelections property. Media selection options for interstitial assets may be circumscribed by -[AVAssetDownloadConfiguration setInterstitialMediaSelectionCriteria: forMediaCharacteristic:].
--
-- - Parameter sampleRate: The RHS value for the sample rate in the predicate equation. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForAudioSampleRate:operatorType:@
predicateForAudioSampleRate_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForAudioSampleRate_operatorType sampleRate operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMessage cls' predicateForAudioSampleRate_operatorTypeSelector sampleRate operatorType

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetVariantQualifier)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetVariantQualifier)
newSelector = mkSelector "new"

-- | @Selector@ for @assetVariantQualifierWithPredicate:@
assetVariantQualifierWithPredicateSelector :: Selector '[Id NSPredicate] (Id AVAssetVariantQualifier)
assetVariantQualifierWithPredicateSelector = mkSelector "assetVariantQualifierWithPredicate:"

-- | @Selector@ for @assetVariantQualifierWithVariant:@
assetVariantQualifierWithVariantSelector :: Selector '[Id AVAssetVariant] (Id AVAssetVariantQualifier)
assetVariantQualifierWithVariantSelector = mkSelector "assetVariantQualifierWithVariant:"

-- | @Selector@ for @assetVariantQualifierForMinimumValueInKeyPath:@
assetVariantQualifierForMinimumValueInKeyPathSelector :: Selector '[Id NSString] (Id AVAssetVariantQualifier)
assetVariantQualifierForMinimumValueInKeyPathSelector = mkSelector "assetVariantQualifierForMinimumValueInKeyPath:"

-- | @Selector@ for @assetVariantQualifierForMaximumValueInKeyPath:@
assetVariantQualifierForMaximumValueInKeyPathSelector :: Selector '[Id NSString] (Id AVAssetVariantQualifier)
assetVariantQualifierForMaximumValueInKeyPathSelector = mkSelector "assetVariantQualifierForMaximumValueInKeyPath:"

-- | @Selector@ for @predicateForChannelCount:mediaSelectionOption:operatorType:@
predicateForChannelCount_mediaSelectionOption_operatorTypeSelector :: Selector '[CLong, Id AVMediaSelectionOption, NSPredicateOperatorType] (Id NSPredicate)
predicateForChannelCount_mediaSelectionOption_operatorTypeSelector = mkSelector "predicateForChannelCount:mediaSelectionOption:operatorType:"

-- | @Selector@ for @predicateForBinauralAudio:mediaSelectionOption:@
predicateForBinauralAudio_mediaSelectionOptionSelector :: Selector '[Bool, Id AVMediaSelectionOption] (Id NSPredicate)
predicateForBinauralAudio_mediaSelectionOptionSelector = mkSelector "predicateForBinauralAudio:mediaSelectionOption:"

-- | @Selector@ for @predicateForImmersiveAudio:mediaSelectionOption:@
predicateForImmersiveAudio_mediaSelectionOptionSelector :: Selector '[Bool, Id AVMediaSelectionOption] (Id NSPredicate)
predicateForImmersiveAudio_mediaSelectionOptionSelector = mkSelector "predicateForImmersiveAudio:mediaSelectionOption:"

-- | @Selector@ for @predicateForDownmixAudio:mediaSelectionOption:@
predicateForDownmixAudio_mediaSelectionOptionSelector :: Selector '[Bool, Id AVMediaSelectionOption] (Id NSPredicate)
predicateForDownmixAudio_mediaSelectionOptionSelector = mkSelector "predicateForDownmixAudio:mediaSelectionOption:"

-- | @Selector@ for @predicateForPresentationWidth:operatorType:@
predicateForPresentationWidth_operatorTypeSelector :: Selector '[CDouble, NSPredicateOperatorType] (Id NSPredicate)
predicateForPresentationWidth_operatorTypeSelector = mkSelector "predicateForPresentationWidth:operatorType:"

-- | @Selector@ for @predicateForPresentationHeight:operatorType:@
predicateForPresentationHeight_operatorTypeSelector :: Selector '[CDouble, NSPredicateOperatorType] (Id NSPredicate)
predicateForPresentationHeight_operatorTypeSelector = mkSelector "predicateForPresentationHeight:operatorType:"

-- | @Selector@ for @predicateForAudioSampleRate:mediaSelectionOption:operatorType:@
predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector :: Selector '[CDouble, Id AVMediaSelectionOption, NSPredicateOperatorType] (Id NSPredicate)
predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector = mkSelector "predicateForAudioSampleRate:mediaSelectionOption:operatorType:"

-- | @Selector@ for @predicateForChannelCount:operatorType:@
predicateForChannelCount_operatorTypeSelector :: Selector '[CLong, NSPredicateOperatorType] (Id NSPredicate)
predicateForChannelCount_operatorTypeSelector = mkSelector "predicateForChannelCount:operatorType:"

-- | @Selector@ for @predicateForBinauralAudio:@
predicateForBinauralAudioSelector :: Selector '[Bool] (Id NSPredicate)
predicateForBinauralAudioSelector = mkSelector "predicateForBinauralAudio:"

-- | @Selector@ for @predicateForImmersiveAudio:@
predicateForImmersiveAudioSelector :: Selector '[Bool] (Id NSPredicate)
predicateForImmersiveAudioSelector = mkSelector "predicateForImmersiveAudio:"

-- | @Selector@ for @predicateForDownmixAudio:@
predicateForDownmixAudioSelector :: Selector '[Bool] (Id NSPredicate)
predicateForDownmixAudioSelector = mkSelector "predicateForDownmixAudio:"

-- | @Selector@ for @predicateForAudioSampleRate:operatorType:@
predicateForAudioSampleRate_operatorTypeSelector :: Selector '[CDouble, NSPredicateOperatorType] (Id NSPredicate)
predicateForAudioSampleRate_operatorTypeSelector = mkSelector "predicateForAudioSampleRate:operatorType:"

