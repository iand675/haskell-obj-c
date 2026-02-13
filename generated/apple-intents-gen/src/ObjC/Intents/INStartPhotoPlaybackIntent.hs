{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartPhotoPlaybackIntent@.
module ObjC.Intents.INStartPhotoPlaybackIntent
  ( INStartPhotoPlaybackIntent
  , IsINStartPhotoPlaybackIntent(..)
  , initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto
  , dateCreated
  , locationCreated
  , albumName
  , searchTerms
  , searchTermsOperator
  , includedAttributes
  , excludedAttributes
  , peopleInPhoto
  , peopleInPhotoOperator
  , albumNameSelector
  , dateCreatedSelector
  , excludedAttributesSelector
  , includedAttributesSelector
  , initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector
  , locationCreatedSelector
  , peopleInPhotoOperatorSelector
  , peopleInPhotoSelector
  , searchTermsOperatorSelector
  , searchTermsSelector

  -- * Enum types
  , INConditionalOperator(INConditionalOperator)
  , pattern INConditionalOperatorAll
  , pattern INConditionalOperatorAny
  , pattern INConditionalOperatorNone
  , INPhotoAttributeOptions(INPhotoAttributeOptions)
  , pattern INPhotoAttributeOptionPhoto
  , pattern INPhotoAttributeOptionVideo
  , pattern INPhotoAttributeOptionGIF
  , pattern INPhotoAttributeOptionFlash
  , pattern INPhotoAttributeOptionLandscapeOrientation
  , pattern INPhotoAttributeOptionPortraitOrientation
  , pattern INPhotoAttributeOptionFavorite
  , pattern INPhotoAttributeOptionSelfie
  , pattern INPhotoAttributeOptionFrontFacingCamera
  , pattern INPhotoAttributeOptionScreenshot
  , pattern INPhotoAttributeOptionBurstPhoto
  , pattern INPhotoAttributeOptionHDRPhoto
  , pattern INPhotoAttributeOptionSquarePhoto
  , pattern INPhotoAttributeOptionPanoramaPhoto
  , pattern INPhotoAttributeOptionTimeLapseVideo
  , pattern INPhotoAttributeOptionSlowMotionVideo
  , pattern INPhotoAttributeOptionNoirFilter
  , pattern INPhotoAttributeOptionChromeFilter
  , pattern INPhotoAttributeOptionInstantFilter
  , pattern INPhotoAttributeOptionTonalFilter
  , pattern INPhotoAttributeOptionTransferFilter
  , pattern INPhotoAttributeOptionMonoFilter
  , pattern INPhotoAttributeOptionFadeFilter
  , pattern INPhotoAttributeOptionProcessFilter
  , pattern INPhotoAttributeOptionPortraitPhoto
  , pattern INPhotoAttributeOptionLivePhoto
  , pattern INPhotoAttributeOptionLoopPhoto
  , pattern INPhotoAttributeOptionBouncePhoto
  , pattern INPhotoAttributeOptionLongExposurePhoto

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:@
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto :: (IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent, IsINDateComponentsRange dateCreated, IsCLPlacemark locationCreated, IsNSString albumName, IsNSArray searchTerms, IsNSArray peopleInPhoto) => inStartPhotoPlaybackIntent -> dateCreated -> locationCreated -> albumName -> searchTerms -> INPhotoAttributeOptions -> INPhotoAttributeOptions -> peopleInPhoto -> IO (Id INStartPhotoPlaybackIntent)
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto inStartPhotoPlaybackIntent dateCreated locationCreated albumName searchTerms includedAttributes excludedAttributes peopleInPhoto =
  sendOwnedMessage inStartPhotoPlaybackIntent initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector (toINDateComponentsRange dateCreated) (toCLPlacemark locationCreated) (toNSString albumName) (toNSArray searchTerms) includedAttributes excludedAttributes (toNSArray peopleInPhoto)

-- | @- dateCreated@
dateCreated :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id INDateComponentsRange)
dateCreated inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent dateCreatedSelector

-- | @- locationCreated@
locationCreated :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id CLPlacemark)
locationCreated inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent locationCreatedSelector

-- | @- albumName@
albumName :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id NSString)
albumName inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent albumNameSelector

-- | @- searchTerms@
searchTerms :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id NSArray)
searchTerms inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent searchTermsSelector

-- | @- searchTermsOperator@
searchTermsOperator :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INConditionalOperator
searchTermsOperator inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent searchTermsOperatorSelector

-- | @- includedAttributes@
includedAttributes :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INPhotoAttributeOptions
includedAttributes inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent includedAttributesSelector

-- | @- excludedAttributes@
excludedAttributes :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INPhotoAttributeOptions
excludedAttributes inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent excludedAttributesSelector

-- | @- peopleInPhoto@
peopleInPhoto :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id NSArray)
peopleInPhoto inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent peopleInPhotoSelector

-- | @- peopleInPhotoOperator@
peopleInPhotoOperator :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INConditionalOperator
peopleInPhotoOperator inStartPhotoPlaybackIntent =
  sendMessage inStartPhotoPlaybackIntent peopleInPhotoOperatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:@
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector :: Selector '[Id INDateComponentsRange, Id CLPlacemark, Id NSString, Id NSArray, INPhotoAttributeOptions, INPhotoAttributeOptions, Id NSArray] (Id INStartPhotoPlaybackIntent)
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector = mkSelector "initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector '[] (Id INDateComponentsRange)
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @locationCreated@
locationCreatedSelector :: Selector '[] (Id CLPlacemark)
locationCreatedSelector = mkSelector "locationCreated"

-- | @Selector@ for @albumName@
albumNameSelector :: Selector '[] (Id NSString)
albumNameSelector = mkSelector "albumName"

-- | @Selector@ for @searchTerms@
searchTermsSelector :: Selector '[] (Id NSArray)
searchTermsSelector = mkSelector "searchTerms"

-- | @Selector@ for @searchTermsOperator@
searchTermsOperatorSelector :: Selector '[] INConditionalOperator
searchTermsOperatorSelector = mkSelector "searchTermsOperator"

-- | @Selector@ for @includedAttributes@
includedAttributesSelector :: Selector '[] INPhotoAttributeOptions
includedAttributesSelector = mkSelector "includedAttributes"

-- | @Selector@ for @excludedAttributes@
excludedAttributesSelector :: Selector '[] INPhotoAttributeOptions
excludedAttributesSelector = mkSelector "excludedAttributes"

-- | @Selector@ for @peopleInPhoto@
peopleInPhotoSelector :: Selector '[] (Id NSArray)
peopleInPhotoSelector = mkSelector "peopleInPhoto"

-- | @Selector@ for @peopleInPhotoOperator@
peopleInPhotoOperatorSelector :: Selector '[] INConditionalOperator
peopleInPhotoOperatorSelector = mkSelector "peopleInPhotoOperator"

