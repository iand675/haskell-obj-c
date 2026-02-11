{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector
  , dateCreatedSelector
  , locationCreatedSelector
  , albumNameSelector
  , searchTermsSelector
  , searchTermsOperatorSelector
  , includedAttributesSelector
  , excludedAttributesSelector
  , peopleInPhotoSelector
  , peopleInPhotoOperatorSelector

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:@
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto :: (IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent, IsINDateComponentsRange dateCreated, IsCLPlacemark locationCreated, IsNSString albumName, IsNSArray searchTerms, IsNSArray peopleInPhoto) => inStartPhotoPlaybackIntent -> dateCreated -> locationCreated -> albumName -> searchTerms -> INPhotoAttributeOptions -> INPhotoAttributeOptions -> peopleInPhoto -> IO (Id INStartPhotoPlaybackIntent)
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhoto inStartPhotoPlaybackIntent  dateCreated locationCreated albumName searchTerms includedAttributes excludedAttributes peopleInPhoto =
withObjCPtr dateCreated $ \raw_dateCreated ->
  withObjCPtr locationCreated $ \raw_locationCreated ->
    withObjCPtr albumName $ \raw_albumName ->
      withObjCPtr searchTerms $ \raw_searchTerms ->
        withObjCPtr peopleInPhoto $ \raw_peopleInPhoto ->
            sendMsg inStartPhotoPlaybackIntent (mkSelector "initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:") (retPtr retVoid) [argPtr (castPtr raw_dateCreated :: Ptr ()), argPtr (castPtr raw_locationCreated :: Ptr ()), argPtr (castPtr raw_albumName :: Ptr ()), argPtr (castPtr raw_searchTerms :: Ptr ()), argCULong (coerce includedAttributes), argCULong (coerce excludedAttributes), argPtr (castPtr raw_peopleInPhoto :: Ptr ())] >>= ownedObject . castPtr

-- | @- dateCreated@
dateCreated :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id INDateComponentsRange)
dateCreated inStartPhotoPlaybackIntent  =
  sendMsg inStartPhotoPlaybackIntent (mkSelector "dateCreated") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- locationCreated@
locationCreated :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id CLPlacemark)
locationCreated inStartPhotoPlaybackIntent  =
  sendMsg inStartPhotoPlaybackIntent (mkSelector "locationCreated") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- albumName@
albumName :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id NSString)
albumName inStartPhotoPlaybackIntent  =
  sendMsg inStartPhotoPlaybackIntent (mkSelector "albumName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- searchTerms@
searchTerms :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id NSArray)
searchTerms inStartPhotoPlaybackIntent  =
  sendMsg inStartPhotoPlaybackIntent (mkSelector "searchTerms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- searchTermsOperator@
searchTermsOperator :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INConditionalOperator
searchTermsOperator inStartPhotoPlaybackIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inStartPhotoPlaybackIntent (mkSelector "searchTermsOperator") retCLong []

-- | @- includedAttributes@
includedAttributes :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INPhotoAttributeOptions
includedAttributes inStartPhotoPlaybackIntent  =
  fmap (coerce :: CULong -> INPhotoAttributeOptions) $ sendMsg inStartPhotoPlaybackIntent (mkSelector "includedAttributes") retCULong []

-- | @- excludedAttributes@
excludedAttributes :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INPhotoAttributeOptions
excludedAttributes inStartPhotoPlaybackIntent  =
  fmap (coerce :: CULong -> INPhotoAttributeOptions) $ sendMsg inStartPhotoPlaybackIntent (mkSelector "excludedAttributes") retCULong []

-- | @- peopleInPhoto@
peopleInPhoto :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO (Id NSArray)
peopleInPhoto inStartPhotoPlaybackIntent  =
  sendMsg inStartPhotoPlaybackIntent (mkSelector "peopleInPhoto") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- peopleInPhotoOperator@
peopleInPhotoOperator :: IsINStartPhotoPlaybackIntent inStartPhotoPlaybackIntent => inStartPhotoPlaybackIntent -> IO INConditionalOperator
peopleInPhotoOperator inStartPhotoPlaybackIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inStartPhotoPlaybackIntent (mkSelector "peopleInPhotoOperator") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:@
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector :: Selector
initWithDateCreated_locationCreated_albumName_searchTerms_includedAttributes_excludedAttributes_peopleInPhotoSelector = mkSelector "initWithDateCreated:locationCreated:albumName:searchTerms:includedAttributes:excludedAttributes:peopleInPhoto:"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @locationCreated@
locationCreatedSelector :: Selector
locationCreatedSelector = mkSelector "locationCreated"

-- | @Selector@ for @albumName@
albumNameSelector :: Selector
albumNameSelector = mkSelector "albumName"

-- | @Selector@ for @searchTerms@
searchTermsSelector :: Selector
searchTermsSelector = mkSelector "searchTerms"

-- | @Selector@ for @searchTermsOperator@
searchTermsOperatorSelector :: Selector
searchTermsOperatorSelector = mkSelector "searchTermsOperator"

-- | @Selector@ for @includedAttributes@
includedAttributesSelector :: Selector
includedAttributesSelector = mkSelector "includedAttributes"

-- | @Selector@ for @excludedAttributes@
excludedAttributesSelector :: Selector
excludedAttributesSelector = mkSelector "excludedAttributes"

-- | @Selector@ for @peopleInPhoto@
peopleInPhotoSelector :: Selector
peopleInPhotoSelector = mkSelector "peopleInPhoto"

-- | @Selector@ for @peopleInPhotoOperator@
peopleInPhotoOperatorSelector :: Selector
peopleInPhotoOperatorSelector = mkSelector "peopleInPhotoOperator"

