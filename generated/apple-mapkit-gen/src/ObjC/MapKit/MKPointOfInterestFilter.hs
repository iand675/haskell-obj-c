{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPointOfInterestFilter@.
module ObjC.MapKit.MKPointOfInterestFilter
  ( MKPointOfInterestFilter
  , IsMKPointOfInterestFilter(..)
  , initIncludingCategories
  , initExcludingCategories
  , includesCategory
  , excludesCategory
  , filterIncludingAllCategories
  , filterExcludingAllCategories
  , excludesCategorySelector
  , filterExcludingAllCategoriesSelector
  , filterIncludingAllCategoriesSelector
  , includesCategorySelector
  , initExcludingCategoriesSelector
  , initIncludingCategoriesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initIncludingCategories:@
initIncludingCategories :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSArray categories) => mkPointOfInterestFilter -> categories -> IO (Id MKPointOfInterestFilter)
initIncludingCategories mkPointOfInterestFilter categories =
  sendOwnedMessage mkPointOfInterestFilter initIncludingCategoriesSelector (toNSArray categories)

-- | @- initExcludingCategories:@
initExcludingCategories :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSArray categories) => mkPointOfInterestFilter -> categories -> IO (Id MKPointOfInterestFilter)
initExcludingCategories mkPointOfInterestFilter categories =
  sendOwnedMessage mkPointOfInterestFilter initExcludingCategoriesSelector (toNSArray categories)

-- | @- includesCategory:@
includesCategory :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSString category) => mkPointOfInterestFilter -> category -> IO Bool
includesCategory mkPointOfInterestFilter category =
  sendMessage mkPointOfInterestFilter includesCategorySelector (toNSString category)

-- | @- excludesCategory:@
excludesCategory :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSString category) => mkPointOfInterestFilter -> category -> IO Bool
excludesCategory mkPointOfInterestFilter category =
  sendMessage mkPointOfInterestFilter excludesCategorySelector (toNSString category)

-- | @+ filterIncludingAllCategories@
filterIncludingAllCategories :: IO (Id MKPointOfInterestFilter)
filterIncludingAllCategories  =
  do
    cls' <- getRequiredClass "MKPointOfInterestFilter"
    sendClassMessage cls' filterIncludingAllCategoriesSelector

-- | @+ filterExcludingAllCategories@
filterExcludingAllCategories :: IO (Id MKPointOfInterestFilter)
filterExcludingAllCategories  =
  do
    cls' <- getRequiredClass "MKPointOfInterestFilter"
    sendClassMessage cls' filterExcludingAllCategoriesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initIncludingCategories:@
initIncludingCategoriesSelector :: Selector '[Id NSArray] (Id MKPointOfInterestFilter)
initIncludingCategoriesSelector = mkSelector "initIncludingCategories:"

-- | @Selector@ for @initExcludingCategories:@
initExcludingCategoriesSelector :: Selector '[Id NSArray] (Id MKPointOfInterestFilter)
initExcludingCategoriesSelector = mkSelector "initExcludingCategories:"

-- | @Selector@ for @includesCategory:@
includesCategorySelector :: Selector '[Id NSString] Bool
includesCategorySelector = mkSelector "includesCategory:"

-- | @Selector@ for @excludesCategory:@
excludesCategorySelector :: Selector '[Id NSString] Bool
excludesCategorySelector = mkSelector "excludesCategory:"

-- | @Selector@ for @filterIncludingAllCategories@
filterIncludingAllCategoriesSelector :: Selector '[] (Id MKPointOfInterestFilter)
filterIncludingAllCategoriesSelector = mkSelector "filterIncludingAllCategories"

-- | @Selector@ for @filterExcludingAllCategories@
filterExcludingAllCategoriesSelector :: Selector '[] (Id MKPointOfInterestFilter)
filterExcludingAllCategoriesSelector = mkSelector "filterExcludingAllCategories"

