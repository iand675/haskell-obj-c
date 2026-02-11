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
  , initIncludingCategoriesSelector
  , initExcludingCategoriesSelector
  , includesCategorySelector
  , excludesCategorySelector
  , filterIncludingAllCategoriesSelector
  , filterExcludingAllCategoriesSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initIncludingCategories:@
initIncludingCategories :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSArray categories) => mkPointOfInterestFilter -> categories -> IO (Id MKPointOfInterestFilter)
initIncludingCategories mkPointOfInterestFilter  categories =
withObjCPtr categories $ \raw_categories ->
    sendMsg mkPointOfInterestFilter (mkSelector "initIncludingCategories:") (retPtr retVoid) [argPtr (castPtr raw_categories :: Ptr ())] >>= ownedObject . castPtr

-- | @- initExcludingCategories:@
initExcludingCategories :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSArray categories) => mkPointOfInterestFilter -> categories -> IO (Id MKPointOfInterestFilter)
initExcludingCategories mkPointOfInterestFilter  categories =
withObjCPtr categories $ \raw_categories ->
    sendMsg mkPointOfInterestFilter (mkSelector "initExcludingCategories:") (retPtr retVoid) [argPtr (castPtr raw_categories :: Ptr ())] >>= ownedObject . castPtr

-- | @- includesCategory:@
includesCategory :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSString category) => mkPointOfInterestFilter -> category -> IO Bool
includesCategory mkPointOfInterestFilter  category =
withObjCPtr category $ \raw_category ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkPointOfInterestFilter (mkSelector "includesCategory:") retCULong [argPtr (castPtr raw_category :: Ptr ())]

-- | @- excludesCategory:@
excludesCategory :: (IsMKPointOfInterestFilter mkPointOfInterestFilter, IsNSString category) => mkPointOfInterestFilter -> category -> IO Bool
excludesCategory mkPointOfInterestFilter  category =
withObjCPtr category $ \raw_category ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkPointOfInterestFilter (mkSelector "excludesCategory:") retCULong [argPtr (castPtr raw_category :: Ptr ())]

-- | @+ filterIncludingAllCategories@
filterIncludingAllCategories :: IO (Id MKPointOfInterestFilter)
filterIncludingAllCategories  =
  do
    cls' <- getRequiredClass "MKPointOfInterestFilter"
    sendClassMsg cls' (mkSelector "filterIncludingAllCategories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ filterExcludingAllCategories@
filterExcludingAllCategories :: IO (Id MKPointOfInterestFilter)
filterExcludingAllCategories  =
  do
    cls' <- getRequiredClass "MKPointOfInterestFilter"
    sendClassMsg cls' (mkSelector "filterExcludingAllCategories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initIncludingCategories:@
initIncludingCategoriesSelector :: Selector
initIncludingCategoriesSelector = mkSelector "initIncludingCategories:"

-- | @Selector@ for @initExcludingCategories:@
initExcludingCategoriesSelector :: Selector
initExcludingCategoriesSelector = mkSelector "initExcludingCategories:"

-- | @Selector@ for @includesCategory:@
includesCategorySelector :: Selector
includesCategorySelector = mkSelector "includesCategory:"

-- | @Selector@ for @excludesCategory:@
excludesCategorySelector :: Selector
excludesCategorySelector = mkSelector "excludesCategory:"

-- | @Selector@ for @filterIncludingAllCategories@
filterIncludingAllCategoriesSelector :: Selector
filterIncludingAllCategoriesSelector = mkSelector "filterIncludingAllCategories"

-- | @Selector@ for @filterExcludingAllCategories@
filterExcludingAllCategoriesSelector :: Selector
filterExcludingAllCategoriesSelector = mkSelector "filterExcludingAllCategories"

