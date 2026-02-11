{-# LANGUAGE PatternSynonyms #-}
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
  , fontCollectionWithDescriptorsSelector
  , fontCollectionWithLocaleSelector
  , showFontCollection_withName_visibility_errorSelector
  , hideFontCollectionWithName_visibility_errorSelector
  , renameFontCollectionWithName_visibility_toName_errorSelector
  , fontCollectionWithNameSelector
  , fontCollectionWithName_visibilitySelector
  , matchingDescriptorsWithOptionsSelector
  , matchingDescriptorsForFamilySelector
  , matchingDescriptorsForFamily_optionsSelector
  , fontCollectionWithAllAvailableDescriptorsSelector
  , allFontCollectionNamesSelector
  , queryDescriptorsSelector
  , exclusionDescriptorsSelector
  , matchingDescriptorsSelector

  -- * Enum types
  , NSFontCollectionVisibility(NSFontCollectionVisibility)
  , pattern NSFontCollectionVisibilityProcess
  , pattern NSFontCollectionVisibilityUser
  , pattern NSFontCollectionVisibilityComputer

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ fontCollectionWithDescriptors:@
fontCollectionWithDescriptors :: IsNSArray queryDescriptors => queryDescriptors -> IO (Id NSFontCollection)
fontCollectionWithDescriptors queryDescriptors =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr queryDescriptors $ \raw_queryDescriptors ->
      sendClassMsg cls' (mkSelector "fontCollectionWithDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_queryDescriptors :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontCollectionWithLocale:@
fontCollectionWithLocale :: IsNSLocale locale => locale -> IO (Id NSFontCollection)
fontCollectionWithLocale locale =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr locale $ \raw_locale ->
      sendClassMsg cls' (mkSelector "fontCollectionWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @+ showFontCollection:withName:visibility:error:@
showFontCollection_withName_visibility_error :: (IsNSFontCollection collection, IsNSString name, IsNSError error_) => collection -> name -> NSFontCollectionVisibility -> error_ -> IO Bool
showFontCollection_withName_visibility_error collection name visibility error_ =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr collection $ \raw_collection ->
      withObjCPtr name $ \raw_name ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "showFontCollection:withName:visibility:error:") retCULong [argPtr (castPtr raw_collection :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce visibility), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ hideFontCollectionWithName:visibility:error:@
hideFontCollectionWithName_visibility_error :: (IsNSString name, IsNSError error_) => name -> NSFontCollectionVisibility -> error_ -> IO Bool
hideFontCollectionWithName_visibility_error name visibility error_ =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr name $ \raw_name ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "hideFontCollectionWithName:visibility:error:") retCULong [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce visibility), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ renameFontCollectionWithName:visibility:toName:error:@
renameFontCollectionWithName_visibility_toName_error :: (IsNSString oldName, IsNSString newName, IsNSError outError) => oldName -> NSFontCollectionVisibility -> newName -> outError -> IO Bool
renameFontCollectionWithName_visibility_toName_error oldName visibility newName outError =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr oldName $ \raw_oldName ->
      withObjCPtr newName $ \raw_newName ->
        withObjCPtr outError $ \raw_outError ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "renameFontCollectionWithName:visibility:toName:error:") retCULong [argPtr (castPtr raw_oldName :: Ptr ()), argCULong (coerce visibility), argPtr (castPtr raw_newName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @+ fontCollectionWithName:@
fontCollectionWithName :: IsNSString name => name -> IO (Id NSFontCollection)
fontCollectionWithName name =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "fontCollectionWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontCollectionWithName:visibility:@
fontCollectionWithName_visibility :: IsNSString name => name -> NSFontCollectionVisibility -> IO (Id NSFontCollection)
fontCollectionWithName_visibility name visibility =
  do
    cls' <- getRequiredClass "NSFontCollection"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "fontCollectionWithName:visibility:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce visibility)] >>= retainedObject . castPtr

-- | @- matchingDescriptorsWithOptions:@
matchingDescriptorsWithOptions :: (IsNSFontCollection nsFontCollection, IsNSDictionary options) => nsFontCollection -> options -> IO (Id NSArray)
matchingDescriptorsWithOptions nsFontCollection  options =
withObjCPtr options $ \raw_options ->
    sendMsg nsFontCollection (mkSelector "matchingDescriptorsWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- matchingDescriptorsForFamily:@
matchingDescriptorsForFamily :: (IsNSFontCollection nsFontCollection, IsNSString family_) => nsFontCollection -> family_ -> IO (Id NSArray)
matchingDescriptorsForFamily nsFontCollection  family_ =
withObjCPtr family_ $ \raw_family_ ->
    sendMsg nsFontCollection (mkSelector "matchingDescriptorsForFamily:") (retPtr retVoid) [argPtr (castPtr raw_family_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- matchingDescriptorsForFamily:options:@
matchingDescriptorsForFamily_options :: (IsNSFontCollection nsFontCollection, IsNSString family_, IsNSDictionary options) => nsFontCollection -> family_ -> options -> IO (Id NSArray)
matchingDescriptorsForFamily_options nsFontCollection  family_ options =
withObjCPtr family_ $ \raw_family_ ->
  withObjCPtr options $ \raw_options ->
      sendMsg nsFontCollection (mkSelector "matchingDescriptorsForFamily:options:") (retPtr retVoid) [argPtr (castPtr raw_family_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptors :: IO (Id NSFontCollection)
fontCollectionWithAllAvailableDescriptors  =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMsg cls' (mkSelector "fontCollectionWithAllAvailableDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ allFontCollectionNames@
allFontCollectionNames :: IO (Id NSArray)
allFontCollectionNames  =
  do
    cls' <- getRequiredClass "NSFontCollection"
    sendClassMsg cls' (mkSelector "allFontCollectionNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- queryDescriptors@
queryDescriptors :: IsNSFontCollection nsFontCollection => nsFontCollection -> IO (Id NSArray)
queryDescriptors nsFontCollection  =
  sendMsg nsFontCollection (mkSelector "queryDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- exclusionDescriptors@
exclusionDescriptors :: IsNSFontCollection nsFontCollection => nsFontCollection -> IO (Id NSArray)
exclusionDescriptors nsFontCollection  =
  sendMsg nsFontCollection (mkSelector "exclusionDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- matchingDescriptors@
matchingDescriptors :: IsNSFontCollection nsFontCollection => nsFontCollection -> IO (Id NSArray)
matchingDescriptors nsFontCollection  =
  sendMsg nsFontCollection (mkSelector "matchingDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fontCollectionWithDescriptors:@
fontCollectionWithDescriptorsSelector :: Selector
fontCollectionWithDescriptorsSelector = mkSelector "fontCollectionWithDescriptors:"

-- | @Selector@ for @fontCollectionWithLocale:@
fontCollectionWithLocaleSelector :: Selector
fontCollectionWithLocaleSelector = mkSelector "fontCollectionWithLocale:"

-- | @Selector@ for @showFontCollection:withName:visibility:error:@
showFontCollection_withName_visibility_errorSelector :: Selector
showFontCollection_withName_visibility_errorSelector = mkSelector "showFontCollection:withName:visibility:error:"

-- | @Selector@ for @hideFontCollectionWithName:visibility:error:@
hideFontCollectionWithName_visibility_errorSelector :: Selector
hideFontCollectionWithName_visibility_errorSelector = mkSelector "hideFontCollectionWithName:visibility:error:"

-- | @Selector@ for @renameFontCollectionWithName:visibility:toName:error:@
renameFontCollectionWithName_visibility_toName_errorSelector :: Selector
renameFontCollectionWithName_visibility_toName_errorSelector = mkSelector "renameFontCollectionWithName:visibility:toName:error:"

-- | @Selector@ for @fontCollectionWithName:@
fontCollectionWithNameSelector :: Selector
fontCollectionWithNameSelector = mkSelector "fontCollectionWithName:"

-- | @Selector@ for @fontCollectionWithName:visibility:@
fontCollectionWithName_visibilitySelector :: Selector
fontCollectionWithName_visibilitySelector = mkSelector "fontCollectionWithName:visibility:"

-- | @Selector@ for @matchingDescriptorsWithOptions:@
matchingDescriptorsWithOptionsSelector :: Selector
matchingDescriptorsWithOptionsSelector = mkSelector "matchingDescriptorsWithOptions:"

-- | @Selector@ for @matchingDescriptorsForFamily:@
matchingDescriptorsForFamilySelector :: Selector
matchingDescriptorsForFamilySelector = mkSelector "matchingDescriptorsForFamily:"

-- | @Selector@ for @matchingDescriptorsForFamily:options:@
matchingDescriptorsForFamily_optionsSelector :: Selector
matchingDescriptorsForFamily_optionsSelector = mkSelector "matchingDescriptorsForFamily:options:"

-- | @Selector@ for @fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptorsSelector :: Selector
fontCollectionWithAllAvailableDescriptorsSelector = mkSelector "fontCollectionWithAllAvailableDescriptors"

-- | @Selector@ for @allFontCollectionNames@
allFontCollectionNamesSelector :: Selector
allFontCollectionNamesSelector = mkSelector "allFontCollectionNames"

-- | @Selector@ for @queryDescriptors@
queryDescriptorsSelector :: Selector
queryDescriptorsSelector = mkSelector "queryDescriptors"

-- | @Selector@ for @exclusionDescriptors@
exclusionDescriptorsSelector :: Selector
exclusionDescriptorsSelector = mkSelector "exclusionDescriptors"

-- | @Selector@ for @matchingDescriptors@
matchingDescriptorsSelector :: Selector
matchingDescriptorsSelector = mkSelector "matchingDescriptors"

