{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForPhotosIntent@.
module ObjC.Intents.INSearchForPhotosIntent
  ( INSearchForPhotosIntent
  , IsINSearchForPhotosIntent(..)
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
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto :: (IsINSearchForPhotosIntent inSearchForPhotosIntent, IsINDateComponentsRange dateCreated, IsCLPlacemark locationCreated, IsNSString albumName, IsNSArray searchTerms, IsNSArray peopleInPhoto) => inSearchForPhotosIntent -> dateCreated -> locationCreated -> albumName -> searchTerms -> INPhotoAttributeOptions -> INPhotoAttributeOptions -> peopleInPhoto -> IO (Id INSearchForPhotosIntent)
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto inSearchForPhotosIntent dateCreated locationCreated albumName searchTerms includedAttributes excludedAttributes peopleInPhoto =
  sendOwnedMessage inSearchForPhotosIntent initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector (toINDateComponentsRange dateCreated) (toCLPlacemark locationCreated) (toNSString albumName) (toNSArray searchTerms) includedAttributes excludedAttributes (toNSArray peopleInPhoto)

-- | @- dateCreated@
dateCreated :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO (Id INDateComponentsRange)
dateCreated inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent dateCreatedSelector

-- | @- locationCreated@
locationCreated :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO (Id CLPlacemark)
locationCreated inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent locationCreatedSelector

-- | @- albumName@
albumName :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO (Id NSString)
albumName inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent albumNameSelector

-- | @- searchTerms@
searchTerms :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO (Id NSArray)
searchTerms inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent searchTermsSelector

-- | @- searchTermsOperator@
searchTermsOperator :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO INConditionalOperator
searchTermsOperator inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent searchTermsOperatorSelector

-- | @- includedAttributes@
includedAttributes :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO INPhotoAttributeOptions
includedAttributes inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent includedAttributesSelector

-- | @- excludedAttributes@
excludedAttributes :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO INPhotoAttributeOptions
excludedAttributes inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent excludedAttributesSelector

-- | @- peopleInPhoto@
peopleInPhoto :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO (Id NSArray)
peopleInPhoto inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent peopleInPhotoSelector

-- | @- peopleInPhotoOperator@
peopleInPhotoOperator :: IsINSearchForPhotosIntent inSearchForPhotosIntent => inSearchForPhotosIntent -> IO INConditionalOperator
peopleInPhotoOperator inSearchForPhotosIntent =
  sendMessage inSearchForPhotosIntent peopleInPhotoOperatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:@
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector :: Selector '[Id INDateComponentsRange, Id CLPlacemark, Id NSString, Id NSArray, INPhotoAttributeOptions, INPhotoAttributeOptions, Id NSArray] (Id INSearchForPhotosIntent)
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

