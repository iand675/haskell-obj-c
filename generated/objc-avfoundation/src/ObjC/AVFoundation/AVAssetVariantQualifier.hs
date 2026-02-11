{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , assetVariantQualifierWithPredicateSelector
  , assetVariantQualifierWithVariantSelector
  , assetVariantQualifierForMinimumValueInKeyPathSelector
  , assetVariantQualifierForMaximumValueInKeyPathSelector
  , predicateForChannelCount_mediaSelectionOption_operatorTypeSelector
  , predicateForBinauralAudio_mediaSelectionOptionSelector
  , predicateForImmersiveAudio_mediaSelectionOptionSelector
  , predicateForDownmixAudio_mediaSelectionOptionSelector
  , predicateForPresentationWidth_operatorTypeSelector
  , predicateForPresentationHeight_operatorTypeSelector
  , predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector
  , predicateForChannelCount_operatorTypeSelector
  , predicateForBinauralAudioSelector
  , predicateForImmersiveAudioSelector
  , predicateForDownmixAudioSelector
  , predicateForAudioSampleRate_operatorTypeSelector

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetVariantQualifier avAssetVariantQualifier => avAssetVariantQualifier -> IO (Id AVAssetVariantQualifier)
init_ avAssetVariantQualifier  =
  sendMsg avAssetVariantQualifier (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetVariantQualifier)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns a qualifer for a predicate.
--
-- - Parameter predicate: The variant predicate. Must be a valid, non-nil NSPredicate.
--
-- ObjC selector: @+ assetVariantQualifierWithPredicate:@
assetVariantQualifierWithPredicate :: IsNSPredicate predicate => predicate -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierWithPredicate predicate =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr predicate $ \raw_predicate ->
      sendClassMsg cls' (mkSelector "assetVariantQualifierWithPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a qualifer for a particular asset variant.
--
-- - Parameter variant: A variant obtained from the -[AVAsset variants] or -[AVAssetDownloadConfiguration playableVariants]. Must be a valid, non-nil AVAssetVariant.
--
-- ObjC selector: @+ assetVariantQualifierWithVariant:@
assetVariantQualifierWithVariant :: IsAVAssetVariant variant => variant -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierWithVariant variant =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr variant $ \raw_variant ->
      sendClassMsg cls' (mkSelector "assetVariantQualifierWithVariant:") (retPtr retVoid) [argPtr (castPtr raw_variant :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a qualifer for finding variant with minimum value in the input key path.
--
-- - Parameter keyPath: AVAssetVariant keyPath. Allowed keyPath values are peakBitRate, averageBitRate, videoAttributes.presentationSize. Must be a valid, non-nil NSString.
--
-- ObjC selector: @+ assetVariantQualifierForMinimumValueInKeyPath:@
assetVariantQualifierForMinimumValueInKeyPath :: IsNSString keyPath => keyPath -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierForMinimumValueInKeyPath keyPath =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr keyPath $ \raw_keyPath ->
      sendClassMsg cls' (mkSelector "assetVariantQualifierForMinimumValueInKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a qualifer for finding variant with maximum value in the input key path
--
-- - Parameter keyPath: AVAssetVariant keyPath. Allowed keyPath values are peakBitRate, averageBitRate, videoAttributes.presentationSize. Must be a valid, non-nil NSString.
--
-- ObjC selector: @+ assetVariantQualifierForMaximumValueInKeyPath:@
assetVariantQualifierForMaximumValueInKeyPath :: IsNSString keyPath => keyPath -> IO (Id AVAssetVariantQualifier)
assetVariantQualifierForMaximumValueInKeyPath keyPath =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr keyPath $ \raw_keyPath ->
      sendClassMsg cls' (mkSelector "assetVariantQualifierForMaximumValueInKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a NSPredicate for audio channel count which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter channelCount: The RHS value for the channel count in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForChannelCount:mediaSelectionOption:operatorType:@
predicateForChannelCount_mediaSelectionOption_operatorType :: IsAVMediaSelectionOption mediaSelectionOption => CLong -> mediaSelectionOption -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForChannelCount_mediaSelectionOption_operatorType channelCount mediaSelectionOption operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
      sendClassMsg cls' (mkSelector "predicateForChannelCount:mediaSelectionOption:operatorType:") (retPtr retVoid) [argCLong (fromIntegral channelCount), argPtr (castPtr raw_mediaSelectionOption :: Ptr ()), argCULong (coerce operatorType)] >>= retainedObject . castPtr

-- | Creates a NSPredicate for binaural which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isBinaural: The RHS value for the value of isBinauralAudio in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration.
--
-- ObjC selector: @+ predicateForBinauralAudio:mediaSelectionOption:@
predicateForBinauralAudio_mediaSelectionOption :: IsAVMediaSelectionOption mediaSelectionOption => Bool -> mediaSelectionOption -> IO (Id NSPredicate)
predicateForBinauralAudio_mediaSelectionOption isBinauralAudio mediaSelectionOption =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
      sendClassMsg cls' (mkSelector "predicateForBinauralAudio:mediaSelectionOption:") (retPtr retVoid) [argCULong (if isBinauralAudio then 1 else 0), argPtr (castPtr raw_mediaSelectionOption :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a NSPredicate for immersive audio which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isImmersiveAudio: The RHS value for the value of isImmersiveAudio in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration.
--
-- ObjC selector: @+ predicateForImmersiveAudio:mediaSelectionOption:@
predicateForImmersiveAudio_mediaSelectionOption :: IsAVMediaSelectionOption mediaSelectionOption => Bool -> mediaSelectionOption -> IO (Id NSPredicate)
predicateForImmersiveAudio_mediaSelectionOption isImmersiveAudio mediaSelectionOption =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
      sendClassMsg cls' (mkSelector "predicateForImmersiveAudio:mediaSelectionOption:") (retPtr retVoid) [argCULong (if isImmersiveAudio then 1 else 0), argPtr (castPtr raw_mediaSelectionOption :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a NSPredicate for immersive audio which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isDownmixAudio: The RHS value for the value of isDownmixAudio in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration.
--
-- ObjC selector: @+ predicateForDownmixAudio:mediaSelectionOption:@
predicateForDownmixAudio_mediaSelectionOption :: IsAVMediaSelectionOption mediaSelectionOption => Bool -> mediaSelectionOption -> IO (Id NSPredicate)
predicateForDownmixAudio_mediaSelectionOption isDownmixAudio mediaSelectionOption =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
      sendClassMsg cls' (mkSelector "predicateForDownmixAudio:mediaSelectionOption:") (retPtr retVoid) [argCULong (if isDownmixAudio then 1 else 0), argPtr (castPtr raw_mediaSelectionOption :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a NSPredicate for presentation size width which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter width: The RHS value for the presentation size width in the predicate equation. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForPresentationWidth:operatorType:@
predicateForPresentationWidth_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForPresentationWidth_operatorType width operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMsg cls' (mkSelector "predicateForPresentationWidth:operatorType:") (retPtr retVoid) [argCDouble (fromIntegral width), argCULong (coerce operatorType)] >>= retainedObject . castPtr

-- | Creates a NSPredicate for presentation size height which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter height: The RHS value for the presentation size height in the predicate equation. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForPresentationHeight:operatorType:@
predicateForPresentationHeight_operatorType :: CDouble -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForPresentationHeight_operatorType height operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMsg cls' (mkSelector "predicateForPresentationHeight:operatorType:") (retPtr retVoid) [argCDouble (fromIntegral height), argCULong (coerce operatorType)] >>= retainedObject . castPtr

-- | Creates a NSPredicate for audio sample rate which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter sampleRate: The RHS value for the sample rate in the predicate equation. - Parameter mediaSelectionOption: The audio media selection option under consideration. - Parameter operatorType: The valid values are NSLessThanPredicateOperatorType, NSLessThanOrEqualToPredicateOperatorType, NSGreaterThanPredicateOperatorType, NSGreaterThanOrEqualToPredicateOperatorType, NSEqualToPredicateOperatorType and NSNotEqualToPredicateOperatorType.
--
-- ObjC selector: @+ predicateForAudioSampleRate:mediaSelectionOption:operatorType:@
predicateForAudioSampleRate_mediaSelectionOption_operatorType :: IsAVMediaSelectionOption mediaSelectionOption => CDouble -> mediaSelectionOption -> NSPredicateOperatorType -> IO (Id NSPredicate)
predicateForAudioSampleRate_mediaSelectionOption_operatorType sampleRate mediaSelectionOption operatorType =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
      sendClassMsg cls' (mkSelector "predicateForAudioSampleRate:mediaSelectionOption:operatorType:") (retPtr retVoid) [argCDouble (fromIntegral sampleRate), argPtr (castPtr raw_mediaSelectionOption :: Ptr ()), argCULong (coerce operatorType)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "predicateForChannelCount:operatorType:") (retPtr retVoid) [argCLong (fromIntegral channelCount), argCULong (coerce operatorType)] >>= retainedObject . castPtr

-- | Creates a NSPredicate for binaural which can be used with other NSPredicates to express variant preferences.
--
-- - Parameter isBinaural: The RHS value for the value of isBinauralAudio in the predicate equation.
--
-- ObjC selector: @+ predicateForBinauralAudio:@
predicateForBinauralAudio :: Bool -> IO (Id NSPredicate)
predicateForBinauralAudio isBinauralAudio =
  do
    cls' <- getRequiredClass "AVAssetVariantQualifier"
    sendClassMsg cls' (mkSelector "predicateForBinauralAudio:") (retPtr retVoid) [argCULong (if isBinauralAudio then 1 else 0)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "predicateForImmersiveAudio:") (retPtr retVoid) [argCULong (if isImmersiveAudio then 1 else 0)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "predicateForDownmixAudio:") (retPtr retVoid) [argCULong (if isDownmixAudio then 1 else 0)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "predicateForAudioSampleRate:operatorType:") (retPtr retVoid) [argCDouble (fromIntegral sampleRate), argCULong (coerce operatorType)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @assetVariantQualifierWithPredicate:@
assetVariantQualifierWithPredicateSelector :: Selector
assetVariantQualifierWithPredicateSelector = mkSelector "assetVariantQualifierWithPredicate:"

-- | @Selector@ for @assetVariantQualifierWithVariant:@
assetVariantQualifierWithVariantSelector :: Selector
assetVariantQualifierWithVariantSelector = mkSelector "assetVariantQualifierWithVariant:"

-- | @Selector@ for @assetVariantQualifierForMinimumValueInKeyPath:@
assetVariantQualifierForMinimumValueInKeyPathSelector :: Selector
assetVariantQualifierForMinimumValueInKeyPathSelector = mkSelector "assetVariantQualifierForMinimumValueInKeyPath:"

-- | @Selector@ for @assetVariantQualifierForMaximumValueInKeyPath:@
assetVariantQualifierForMaximumValueInKeyPathSelector :: Selector
assetVariantQualifierForMaximumValueInKeyPathSelector = mkSelector "assetVariantQualifierForMaximumValueInKeyPath:"

-- | @Selector@ for @predicateForChannelCount:mediaSelectionOption:operatorType:@
predicateForChannelCount_mediaSelectionOption_operatorTypeSelector :: Selector
predicateForChannelCount_mediaSelectionOption_operatorTypeSelector = mkSelector "predicateForChannelCount:mediaSelectionOption:operatorType:"

-- | @Selector@ for @predicateForBinauralAudio:mediaSelectionOption:@
predicateForBinauralAudio_mediaSelectionOptionSelector :: Selector
predicateForBinauralAudio_mediaSelectionOptionSelector = mkSelector "predicateForBinauralAudio:mediaSelectionOption:"

-- | @Selector@ for @predicateForImmersiveAudio:mediaSelectionOption:@
predicateForImmersiveAudio_mediaSelectionOptionSelector :: Selector
predicateForImmersiveAudio_mediaSelectionOptionSelector = mkSelector "predicateForImmersiveAudio:mediaSelectionOption:"

-- | @Selector@ for @predicateForDownmixAudio:mediaSelectionOption:@
predicateForDownmixAudio_mediaSelectionOptionSelector :: Selector
predicateForDownmixAudio_mediaSelectionOptionSelector = mkSelector "predicateForDownmixAudio:mediaSelectionOption:"

-- | @Selector@ for @predicateForPresentationWidth:operatorType:@
predicateForPresentationWidth_operatorTypeSelector :: Selector
predicateForPresentationWidth_operatorTypeSelector = mkSelector "predicateForPresentationWidth:operatorType:"

-- | @Selector@ for @predicateForPresentationHeight:operatorType:@
predicateForPresentationHeight_operatorTypeSelector :: Selector
predicateForPresentationHeight_operatorTypeSelector = mkSelector "predicateForPresentationHeight:operatorType:"

-- | @Selector@ for @predicateForAudioSampleRate:mediaSelectionOption:operatorType:@
predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector :: Selector
predicateForAudioSampleRate_mediaSelectionOption_operatorTypeSelector = mkSelector "predicateForAudioSampleRate:mediaSelectionOption:operatorType:"

-- | @Selector@ for @predicateForChannelCount:operatorType:@
predicateForChannelCount_operatorTypeSelector :: Selector
predicateForChannelCount_operatorTypeSelector = mkSelector "predicateForChannelCount:operatorType:"

-- | @Selector@ for @predicateForBinauralAudio:@
predicateForBinauralAudioSelector :: Selector
predicateForBinauralAudioSelector = mkSelector "predicateForBinauralAudio:"

-- | @Selector@ for @predicateForImmersiveAudio:@
predicateForImmersiveAudioSelector :: Selector
predicateForImmersiveAudioSelector = mkSelector "predicateForImmersiveAudio:"

-- | @Selector@ for @predicateForDownmixAudio:@
predicateForDownmixAudioSelector :: Selector
predicateForDownmixAudioSelector = mkSelector "predicateForDownmixAudio:"

-- | @Selector@ for @predicateForAudioSampleRate:operatorType:@
predicateForAudioSampleRate_operatorTypeSelector :: Selector
predicateForAudioSampleRate_operatorTypeSelector = mkSelector "predicateForAudioSampleRate:operatorType:"

