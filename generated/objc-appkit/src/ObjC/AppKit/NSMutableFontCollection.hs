{-# LANGUAGE PatternSynonyms #-}
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
  , fontCollectionWithDescriptorsSelector
  , fontCollectionWithLocaleSelector
  , fontCollectionWithNameSelector
  , fontCollectionWithName_visibilitySelector
  , addQueryForDescriptorsSelector
  , removeQueryForDescriptorsSelector
  , fontCollectionWithAllAvailableDescriptorsSelector
  , queryDescriptorsSelector
  , setQueryDescriptorsSelector
  , exclusionDescriptorsSelector
  , setExclusionDescriptorsSelector

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
fontCollectionWithDescriptors :: IsNSArray queryDescriptors => queryDescriptors -> IO (Id NSMutableFontCollection)
fontCollectionWithDescriptors queryDescriptors =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    withObjCPtr queryDescriptors $ \raw_queryDescriptors ->
      sendClassMsg cls' (mkSelector "fontCollectionWithDescriptors:") (retPtr retVoid) [argPtr (castPtr raw_queryDescriptors :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontCollectionWithLocale:@
fontCollectionWithLocale :: IsNSLocale locale => locale -> IO (Id NSMutableFontCollection)
fontCollectionWithLocale locale =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    withObjCPtr locale $ \raw_locale ->
      sendClassMsg cls' (mkSelector "fontCollectionWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontCollectionWithName:@
fontCollectionWithName :: IsNSString name => name -> IO (Id NSMutableFontCollection)
fontCollectionWithName name =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "fontCollectionWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontCollectionWithName:visibility:@
fontCollectionWithName_visibility :: IsNSString name => name -> NSFontCollectionVisibility -> IO (Id NSMutableFontCollection)
fontCollectionWithName_visibility name visibility =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "fontCollectionWithName:visibility:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce visibility)] >>= retainedObject . castPtr

-- | @- addQueryForDescriptors:@
addQueryForDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray descriptors) => nsMutableFontCollection -> descriptors -> IO ()
addQueryForDescriptors nsMutableFontCollection  descriptors =
withObjCPtr descriptors $ \raw_descriptors ->
    sendMsg nsMutableFontCollection (mkSelector "addQueryForDescriptors:") retVoid [argPtr (castPtr raw_descriptors :: Ptr ())]

-- | @- removeQueryForDescriptors:@
removeQueryForDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray descriptors) => nsMutableFontCollection -> descriptors -> IO ()
removeQueryForDescriptors nsMutableFontCollection  descriptors =
withObjCPtr descriptors $ \raw_descriptors ->
    sendMsg nsMutableFontCollection (mkSelector "removeQueryForDescriptors:") retVoid [argPtr (castPtr raw_descriptors :: Ptr ())]

-- | @+ fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptors :: IO (Id NSMutableFontCollection)
fontCollectionWithAllAvailableDescriptors  =
  do
    cls' <- getRequiredClass "NSMutableFontCollection"
    sendClassMsg cls' (mkSelector "fontCollectionWithAllAvailableDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- queryDescriptors@
queryDescriptors :: IsNSMutableFontCollection nsMutableFontCollection => nsMutableFontCollection -> IO (Id NSArray)
queryDescriptors nsMutableFontCollection  =
  sendMsg nsMutableFontCollection (mkSelector "queryDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQueryDescriptors:@
setQueryDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray value) => nsMutableFontCollection -> value -> IO ()
setQueryDescriptors nsMutableFontCollection  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableFontCollection (mkSelector "setQueryDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- exclusionDescriptors@
exclusionDescriptors :: IsNSMutableFontCollection nsMutableFontCollection => nsMutableFontCollection -> IO (Id NSArray)
exclusionDescriptors nsMutableFontCollection  =
  sendMsg nsMutableFontCollection (mkSelector "exclusionDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExclusionDescriptors:@
setExclusionDescriptors :: (IsNSMutableFontCollection nsMutableFontCollection, IsNSArray value) => nsMutableFontCollection -> value -> IO ()
setExclusionDescriptors nsMutableFontCollection  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMutableFontCollection (mkSelector "setExclusionDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fontCollectionWithDescriptors:@
fontCollectionWithDescriptorsSelector :: Selector
fontCollectionWithDescriptorsSelector = mkSelector "fontCollectionWithDescriptors:"

-- | @Selector@ for @fontCollectionWithLocale:@
fontCollectionWithLocaleSelector :: Selector
fontCollectionWithLocaleSelector = mkSelector "fontCollectionWithLocale:"

-- | @Selector@ for @fontCollectionWithName:@
fontCollectionWithNameSelector :: Selector
fontCollectionWithNameSelector = mkSelector "fontCollectionWithName:"

-- | @Selector@ for @fontCollectionWithName:visibility:@
fontCollectionWithName_visibilitySelector :: Selector
fontCollectionWithName_visibilitySelector = mkSelector "fontCollectionWithName:visibility:"

-- | @Selector@ for @addQueryForDescriptors:@
addQueryForDescriptorsSelector :: Selector
addQueryForDescriptorsSelector = mkSelector "addQueryForDescriptors:"

-- | @Selector@ for @removeQueryForDescriptors:@
removeQueryForDescriptorsSelector :: Selector
removeQueryForDescriptorsSelector = mkSelector "removeQueryForDescriptors:"

-- | @Selector@ for @fontCollectionWithAllAvailableDescriptors@
fontCollectionWithAllAvailableDescriptorsSelector :: Selector
fontCollectionWithAllAvailableDescriptorsSelector = mkSelector "fontCollectionWithAllAvailableDescriptors"

-- | @Selector@ for @queryDescriptors@
queryDescriptorsSelector :: Selector
queryDescriptorsSelector = mkSelector "queryDescriptors"

-- | @Selector@ for @setQueryDescriptors:@
setQueryDescriptorsSelector :: Selector
setQueryDescriptorsSelector = mkSelector "setQueryDescriptors:"

-- | @Selector@ for @exclusionDescriptors@
exclusionDescriptorsSelector :: Selector
exclusionDescriptorsSelector = mkSelector "exclusionDescriptors"

-- | @Selector@ for @setExclusionDescriptors:@
setExclusionDescriptorsSelector :: Selector
setExclusionDescriptorsSelector = mkSelector "setExclusionDescriptors:"

