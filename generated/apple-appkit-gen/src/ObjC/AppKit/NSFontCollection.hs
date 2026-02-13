{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFontCollection@.
module ObjC.AppKit.NSFontCollection
  ( NSFontCollection
  , IsNSFontCollection(..)
  , fontCollectionWithDescriptors
  , fontCollectionWithLocale
  , showFontCollection_withName_visibility_error
  , hideFontCollectionWithName_visibility_error
  , renameFontCollectionWithName_visibility_toName_error
  , fontCollectionWithName
  , fontCollectionWithName_visibility
  , matchingDescriptorsWithOptions
  , matchingDescriptorsForFamily
  , matchingDescriptorsForFamily_options
  , fontCollectionWithAllAvailableDescriptors
  , allFontCollectionNames
  , queryDescriptors
  , exclusionDescriptors
  , matchingDescriptors
  , allFontCollectionNamesSelector
  , exclusionDescriptorsSelector
  , fontCollectionWithAllAvailableDescriptorsSelector
  , fontCollectionWithDescriptorsSelector
  , fontCollectionWithLocaleSelector
  , fontCollectionWithNameSelector
  , fontCollectionWithName_visibilitySelector
  , hideFontCollectionWithName_visibility_errorSelector
  , matchingDescriptorsForFamilySelector
  , matchingDescriptorsForFamily_optionsSelector
  , matchingDescriptorsSelector
  , matchingDescriptorsWithOptionsSelector
  , queryDescriptorsSelector
  , renameFontCollectionWithName_visibility_toName_errorSelector
  , showFontCollection_withName_visibility_errorSelector

  -- * Enum types
  , NSFontCollectionVisibility(NSFontCollectionVisibility)
  , pattern NSFontCollectionVisibilityProcess
  , pattern NSFontCollectionVisibilityUser
  , pattern NSFontCollectionVisibilityComputer

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ fontCollectionWithDescriptors:@
fontCollectionWithDescriptors :: IsNSArray queryDescriptors => queryDescriptors -> IO (Id NSFontCollection)
fontCollectionWithDescriptors queryDescriptors =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' fontCollectionWithDescriptorsSelector (toNSArray queryDescriptors)

-- | @+ fontCollectionWithLocale:@
fontCollectionWithLocale :: IsNSLocale locale => locale -> IO (Id NSFontCollection)
fontCollectionWithLocale locale =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' fontCollectionWithLocaleSelector (toNSLocale locale)

-- | @+ showFontCollection:withName:visibility:error:@
showFontCollection_withName_visibility_error :: (IsNSFontCollection collection, IsNSString name, IsNSError error_) => collection -> name -> NSFontCollectionVisibility -> error_ -> IO Bool
showFontCollection_withName_visibility_error collection name visibility error_ =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' showFontCollection_withName_visibility_errorSelector (toNSFontCollection collection) (toNSString name) visibility (toNSError error_)

-- | @+ hideFontCollectionWithName:visibility:error:@
hideFontCollectionWithName_visibility_error :: (IsNSString name, IsNSError error_) => name -> NSFontCollectionVisibility -> error_ -> IO Bool
hideFontCollectionWithName_visibility_error name visibility error_ =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' hideFontCollectionWithName_visibility_errorSelector (toNSString name) visibility (toNSError error_)

-- | @+ renameFontCollectionWithName:visibility:toName:error:@
renameFontCollectionWithName_visibility_toName_error :: (IsNSString oldName, IsNSString newName, IsNSError outError) => oldName -> NSFontCollectionVisibility -> newName -> outError -> IO Bool
renameFontCollectionWithName_visibility_toName_error oldName visibility newName outError =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' renameFontCollectionWithName_visibility_toName_errorSelector (toNSString oldName) visibility (toNSString newName) (toNSError outError)

-- | @+ fontCollectionWithName:@
fontCollectionWithName :: IsNSString name => name -> IO (Id NSFontCollection)
fontCollectionWithName name =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' fontCollectionWithNameSelector (toNSString name)

-- | @+ fontCollectionWithName:visibility:@
fontCollectionWithName_visibility :: IsNSString name => name -> NSFontCollectionVisibility -> IO (Id NSFontCollection)
fontCollectionWithName_visibility name visibility =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' fontCollectionWithName_visibilitySelector (toNSString name) visibility

-- | @- matchingDescriptorsWithOptions:@
matchingDescriptorsWithOptions :: (IsNSFontCollection nsFontCollection, IsNSDictionary options) => nsFontCollection -> options -> IO (Id NSArray)
matchingDescriptorsWithOptions nsFontCollection options =
  sendMessage nsFontCollection matchingDescriptorsWithOptionsSelector (toNSDictionary options)

-- | @- matchingDescriptorsForFamily:@
matchingDescriptorsForFamily :: (IsNSFontCollection nsFontCollection, IsNSString family_) => nsFontCollection -> family_ -> IO (Id NSArray)
matchingDescriptorsForFamily nsFontCollection family_ =
  sendMessage nsFontCollection matchingDescriptorsForFamilySelector (toNSString family_)

-- | @- matchingDescriptorsForFamily:options:@
matchingDescriptorsForFamily_options :: (IsNSFontCollection nsFontCollection, IsNSString family_, IsNSDictionary options) => nsFontCollection -> family_ -> options -> IO (Id NSArray)
matchingDescriptorsForFamily_options nsFontCollection family_ options =
  sendMessage nsFontCollection matchingDescriptorsForFamily_optionsSelector (toNSString family_) (toNSDictionary options)

-- | @+ fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptors :: IO (Id NSFontCollection)
fontCollectionWithAllAvailableDescriptors  =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' fontCollectionWithAllAvailableDescriptorsSelector

-- | @+ allFontCollectionNames@
allFontCollectionNames :: IO (Id NSArray)
allFontCollectionNames  =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMessage cls' allFontCollectionNamesSelector

-- | @- queryDescriptors@
queryDescriptors :: IsNSFontCollection nsFontCollection => nsFontCollection -> IO (Id NSArray)
queryDescriptors nsFontCollection =
  sendMessage nsFontCollection queryDescriptorsSelector

-- | @- exclusionDescriptors@
exclusionDescriptors :: IsNSFontCollection nsFontCollection => nsFontCollection -> IO (Id NSArray)
exclusionDescriptors nsFontCollection =
  sendMessage nsFontCollection exclusionDescriptorsSelector

-- | @- matchingDescriptors@
matchingDescriptors :: IsNSFontCollection nsFontCollection => nsFontCollection -> IO (Id NSArray)
matchingDescriptors nsFontCollection =
  sendMessage nsFontCollection matchingDescriptorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fontCollectionWithDescriptors:@
fontCollectionWithDescriptorsSelector :: Selector '[Id NSArray] (Id NSFontCollection)
fontCollectionWithDescriptorsSelector = mkSelector "fontCollectionWithDescriptors:"

-- | @Selector@ for @fontCollectionWithLocale:@
fontCollectionWithLocaleSelector :: Selector '[Id NSLocale] (Id NSFontCollection)
fontCollectionWithLocaleSelector = mkSelector "fontCollectionWithLocale:"

-- | @Selector@ for @showFontCollection:withName:visibility:error:@
showFontCollection_withName_visibility_errorSelector :: Selector '[Id NSFontCollection, Id NSString, NSFontCollectionVisibility, Id NSError] Bool
showFontCollection_withName_visibility_errorSelector = mkSelector "showFontCollection:withName:visibility:error:"

-- | @Selector@ for @hideFontCollectionWithName:visibility:error:@
hideFontCollectionWithName_visibility_errorSelector :: Selector '[Id NSString, NSFontCollectionVisibility, Id NSError] Bool
hideFontCollectionWithName_visibility_errorSelector = mkSelector "hideFontCollectionWithName:visibility:error:"

-- | @Selector@ for @renameFontCollectionWithName:visibility:toName:error:@
renameFontCollectionWithName_visibility_toName_errorSelector :: Selector '[Id NSString, NSFontCollectionVisibility, Id NSString, Id NSError] Bool
renameFontCollectionWithName_visibility_toName_errorSelector = mkSelector "renameFontCollectionWithName:visibility:toName:error:"

-- | @Selector@ for @fontCollectionWithName:@
fontCollectionWithNameSelector :: Selector '[Id NSString] (Id NSFontCollection)
fontCollectionWithNameSelector = mkSelector "fontCollectionWithName:"

-- | @Selector@ for @fontCollectionWithName:visibility:@
fontCollectionWithName_visibilitySelector :: Selector '[Id NSString, NSFontCollectionVisibility] (Id NSFontCollection)
fontCollectionWithName_visibilitySelector = mkSelector "fontCollectionWithName:visibility:"

-- | @Selector@ for @matchingDescriptorsWithOptions:@
matchingDescriptorsWithOptionsSelector :: Selector '[Id NSDictionary] (Id NSArray)
matchingDescriptorsWithOptionsSelector = mkSelector "matchingDescriptorsWithOptions:"

-- | @Selector@ for @matchingDescriptorsForFamily:@
matchingDescriptorsForFamilySelector :: Selector '[Id NSString] (Id NSArray)
matchingDescriptorsForFamilySelector = mkSelector "matchingDescriptorsForFamily:"

-- | @Selector@ for @matchingDescriptorsForFamily:options:@
matchingDescriptorsForFamily_optionsSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSArray)
matchingDescriptorsForFamily_optionsSelector = mkSelector "matchingDescriptorsForFamily:options:"

-- | @Selector@ for @fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptorsSelector :: Selector '[] (Id NSFontCollection)
fontCollectionWithAllAvailableDescriptorsSelector = mkSelector "fontCollectionWithAllAvailableDescriptors"

-- | @Selector@ for @allFontCollectionNames@
allFontCollectionNamesSelector :: Selector '[] (Id NSArray)
allFontCollectionNamesSelector = mkSelector "allFontCollectionNames"

-- | @Selector@ for @queryDescriptors@
queryDescriptorsSelector :: Selector '[] (Id NSArray)
queryDescriptorsSelector = mkSelector "queryDescriptors"

-- | @Selector@ for @exclusionDescriptors@
exclusionDescriptorsSelector :: Selector '[] (Id NSArray)
exclusionDescriptorsSelector = mkSelector "exclusionDescriptors"

-- | @Selector@ for @matchingDescriptors@
matchingDescriptorsSelector :: Selector '[] (Id NSArray)
matchingDescriptorsSelector = mkSelector "matchingDescriptors"

