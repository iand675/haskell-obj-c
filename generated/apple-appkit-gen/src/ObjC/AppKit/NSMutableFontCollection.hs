{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableFontCollection@.
module ObjC.AppKit.NSMutableFontCollection
  ( NSMutableFontCollection
  , IsNSMutableFontCollection(..)
  , fontCollectionWithDescriptors
  , fontCollectionWithLocale
  , fontCollectionWithName
  , fontCollectionWithName_visibility
  , addQueryForDescriptors
  , removeQueryForDescriptors
  , fontCollectionWithAllAvailableDescriptors
  , queryDescriptors
  , setQueryDescriptors
  , exclusionDescriptors
  , setExclusionDescriptors
  , addQueryForDescriptorsSelector
  , exclusionDescriptorsSelector
  , fontCollectionWithAllAvailableDescriptorsSelector
  , fontCollectionWithDescriptorsSelector
  , fontCollectionWithLocaleSelector
  , fontCollectionWithNameSelector
  , fontCollectionWithName_visibilitySelector
  , queryDescriptorsSelector
  , removeQueryForDescriptorsSelector
  , setExclusionDescriptorsSelector
  , setQueryDescriptorsSelector

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
fontCollectionWithDescriptors :: IsNSArray queryDescriptors => queryDescriptors -> IO (Id NSMutableFontCollection)
fontCollectionWithDescriptors queryDescriptors =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    sendClassMessage cls' fontCollectionWithDescriptorsSelector (toNSArray queryDescriptors)

-- | @+ fontCollectionWithLocale:@
fontCollectionWithLocale :: IsNSLocale locale => locale -> IO (Id NSMutableFontCollection)
fontCollectionWithLocale locale =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    sendClassMessage cls' fontCollectionWithLocaleSelector (toNSLocale locale)

-- | @+ fontCollectionWithName:@
fontCollectionWithName :: IsNSString name => name -> IO (Id NSMutableFontCollection)
fontCollectionWithName name =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    sendClassMessage cls' fontCollectionWithNameSelector (toNSString name)

-- | @+ fontCollectionWithName:visibility:@
fontCollectionWithName_visibility :: IsNSString name => name -> NSFontCollectionVisibility -> IO (Id NSMutableFontCollection)
fontCollectionWithName_visibility name visibility =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    sendClassMessage cls' fontCollectionWithName_visibilitySelector (toNSString name) visibility

-- | @- addQueryForDescriptors:@
addQueryForDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray descriptors) => nsMutableFontCollection -> descriptors -> IO ()
addQueryForDescriptors nsMutableFontCollection descriptors =
  sendMessage nsMutableFontCollection addQueryForDescriptorsSelector (toNSArray descriptors)

-- | @- removeQueryForDescriptors:@
removeQueryForDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray descriptors) => nsMutableFontCollection -> descriptors -> IO ()
removeQueryForDescriptors nsMutableFontCollection descriptors =
  sendMessage nsMutableFontCollection removeQueryForDescriptorsSelector (toNSArray descriptors)

-- | @+ fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptors :: IO (Id NSMutableFontCollection)
fontCollectionWithAllAvailableDescriptors  =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    sendClassMessage cls' fontCollectionWithAllAvailableDescriptorsSelector

-- | @- queryDescriptors@
queryDescriptors :: IsNSMutableFontCollection nsMutableFontCollection => nsMutableFontCollection -> IO (Id NSArray)
queryDescriptors nsMutableFontCollection =
  sendMessage nsMutableFontCollection queryDescriptorsSelector

-- | @- setQueryDescriptors:@
setQueryDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray value) => nsMutableFontCollection -> value -> IO ()
setQueryDescriptors nsMutableFontCollection value =
  sendMessage nsMutableFontCollection setQueryDescriptorsSelector (toNSArray value)

-- | @- exclusionDescriptors@
exclusionDescriptors :: IsNSMutableFontCollection nsMutableFontCollection => nsMutableFontCollection -> IO (Id NSArray)
exclusionDescriptors nsMutableFontCollection =
  sendMessage nsMutableFontCollection exclusionDescriptorsSelector

-- | @- setExclusionDescriptors:@
setExclusionDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray value) => nsMutableFontCollection -> value -> IO ()
setExclusionDescriptors nsMutableFontCollection value =
  sendMessage nsMutableFontCollection setExclusionDescriptorsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fontCollectionWithDescriptors:@
fontCollectionWithDescriptorsSelector :: Selector '[Id NSArray] (Id NSMutableFontCollection)
fontCollectionWithDescriptorsSelector = mkSelector "fontCollectionWithDescriptors:"

-- | @Selector@ for @fontCollectionWithLocale:@
fontCollectionWithLocaleSelector :: Selector '[Id NSLocale] (Id NSMutableFontCollection)
fontCollectionWithLocaleSelector = mkSelector "fontCollectionWithLocale:"

-- | @Selector@ for @fontCollectionWithName:@
fontCollectionWithNameSelector :: Selector '[Id NSString] (Id NSMutableFontCollection)
fontCollectionWithNameSelector = mkSelector "fontCollectionWithName:"

-- | @Selector@ for @fontCollectionWithName:visibility:@
fontCollectionWithName_visibilitySelector :: Selector '[Id NSString, NSFontCollectionVisibility] (Id NSMutableFontCollection)
fontCollectionWithName_visibilitySelector = mkSelector "fontCollectionWithName:visibility:"

-- | @Selector@ for @addQueryForDescriptors:@
addQueryForDescriptorsSelector :: Selector '[Id NSArray] ()
addQueryForDescriptorsSelector = mkSelector "addQueryForDescriptors:"

-- | @Selector@ for @removeQueryForDescriptors:@
removeQueryForDescriptorsSelector :: Selector '[Id NSArray] ()
removeQueryForDescriptorsSelector = mkSelector "removeQueryForDescriptors:"

-- | @Selector@ for @fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptorsSelector :: Selector '[] (Id NSMutableFontCollection)
fontCollectionWithAllAvailableDescriptorsSelector = mkSelector "fontCollectionWithAllAvailableDescriptors"

-- | @Selector@ for @queryDescriptors@
queryDescriptorsSelector :: Selector '[] (Id NSArray)
queryDescriptorsSelector = mkSelector "queryDescriptors"

-- | @Selector@ for @setQueryDescriptors:@
setQueryDescriptorsSelector :: Selector '[Id NSArray] ()
setQueryDescriptorsSelector = mkSelector "setQueryDescriptors:"

-- | @Selector@ for @exclusionDescriptors@
exclusionDescriptorsSelector :: Selector '[] (Id NSArray)
exclusionDescriptorsSelector = mkSelector "exclusionDescriptors"

-- | @Selector@ for @setExclusionDescriptors:@
setExclusionDescriptorsSelector :: Selector '[Id NSArray] ()
setExclusionDescriptorsSelector = mkSelector "setExclusionDescriptors:"

