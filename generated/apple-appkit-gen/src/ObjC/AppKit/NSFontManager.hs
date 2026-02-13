{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFontManager@.
module ObjC.AppKit.NSFontManager
  ( NSFontManager
  , IsNSFontManager(..)
  , setFontPanelFactory
  , setFontManagerFactory
  , setSelectedFont_isMultiple
  , setFontMenu
  , fontMenu
  , fontPanel
  , fontWithFamily_traits_weight_size
  , traitsOfFont
  , weightOfFont
  , availableMembersOfFontFamily
  , convertFont
  , convertFont_toSize
  , convertFont_toFace
  , convertFont_toFamily
  , convertFont_toHaveTrait
  , convertFont_toNotHaveTrait
  , convertWeight_ofFont
  , sendAction
  , localizedNameForFamily_face
  , setSelectedAttributes_isMultiple
  , convertAttributes
  , availableFontNamesMatchingFontDescriptor
  , fontDescriptorsInCollection
  , addCollection_options
  , removeCollection
  , addFontDescriptors_toCollection
  , removeFontDescriptor_fromCollection
  , convertFontTraits
  , fontNamed_hasTraits
  , availableFontNamesWithTraits
  , addFontTrait
  , removeFontTrait
  , modifyFontViaPanel
  , modifyFont
  , orderFrontFontPanel
  , orderFrontStylesPanel
  , sharedFontManager
  , multiple
  , selectedFont
  , availableFonts
  , availableFontFamilies
  , enabled
  , setEnabled
  , action
  , setAction
  , delegate
  , setDelegate
  , collectionNames
  , currentFontAction
  , target
  , setTarget
  , actionSelector
  , addCollection_optionsSelector
  , addFontDescriptors_toCollectionSelector
  , addFontTraitSelector
  , availableFontFamiliesSelector
  , availableFontNamesMatchingFontDescriptorSelector
  , availableFontNamesWithTraitsSelector
  , availableFontsSelector
  , availableMembersOfFontFamilySelector
  , collectionNamesSelector
  , convertAttributesSelector
  , convertFontSelector
  , convertFontTraitsSelector
  , convertFont_toFaceSelector
  , convertFont_toFamilySelector
  , convertFont_toHaveTraitSelector
  , convertFont_toNotHaveTraitSelector
  , convertFont_toSizeSelector
  , convertWeight_ofFontSelector
  , currentFontActionSelector
  , delegateSelector
  , enabledSelector
  , fontDescriptorsInCollectionSelector
  , fontMenuSelector
  , fontNamed_hasTraitsSelector
  , fontPanelSelector
  , fontWithFamily_traits_weight_sizeSelector
  , localizedNameForFamily_faceSelector
  , modifyFontSelector
  , modifyFontViaPanelSelector
  , multipleSelector
  , orderFrontFontPanelSelector
  , orderFrontStylesPanelSelector
  , removeCollectionSelector
  , removeFontDescriptor_fromCollectionSelector
  , removeFontTraitSelector
  , selectedFontSelector
  , sendActionSelector
  , setActionSelector
  , setDelegateSelector
  , setEnabledSelector
  , setFontManagerFactorySelector
  , setFontMenuSelector
  , setFontPanelFactorySelector
  , setSelectedAttributes_isMultipleSelector
  , setSelectedFont_isMultipleSelector
  , setTargetSelector
  , sharedFontManagerSelector
  , targetSelector
  , traitsOfFontSelector
  , weightOfFontSelector

  -- * Enum types
  , NSFontAction(NSFontAction)
  , pattern NSNoFontChangeAction
  , pattern NSViaPanelFontAction
  , pattern NSAddTraitFontAction
  , pattern NSSizeUpFontAction
  , pattern NSSizeDownFontAction
  , pattern NSHeavierFontAction
  , pattern NSLighterFontAction
  , pattern NSRemoveTraitFontAction
  , NSFontCollectionOptions(NSFontCollectionOptions)
  , pattern NSFontCollectionApplicationOnlyMask
  , NSFontTraitMask(NSFontTraitMask)
  , pattern NSItalicFontMask
  , pattern NSBoldFontMask
  , pattern NSUnboldFontMask
  , pattern NSNonStandardCharacterSetFontMask
  , pattern NSNarrowFontMask
  , pattern NSExpandedFontMask
  , pattern NSCondensedFontMask
  , pattern NSSmallCapsFontMask
  , pattern NSPosterFontMask
  , pattern NSCompressedFontMask
  , pattern NSFixedPitchFontMask
  , pattern NSUnitalicFontMask

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

-- | @+ setFontPanelFactory:@
setFontPanelFactory :: Class -> IO ()
setFontPanelFactory factoryId =
  do
    cls' <- getRequiredClass "NSFontManager"
    sendClassMessage cls' setFontPanelFactorySelector factoryId

-- | @+ setFontManagerFactory:@
setFontManagerFactory :: Class -> IO ()
setFontManagerFactory factoryId =
  do
    cls' <- getRequiredClass "NSFontManager"
    sendClassMessage cls' setFontManagerFactorySelector factoryId

-- | @- setSelectedFont:isMultiple:@
setSelectedFont_isMultiple :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> Bool -> IO ()
setSelectedFont_isMultiple nsFontManager fontObj flag =
  sendMessage nsFontManager setSelectedFont_isMultipleSelector (toNSFont fontObj) flag

-- | @- setFontMenu:@
setFontMenu :: (IsNSFontManager nsFontManager, IsNSMenu newMenu) => nsFontManager -> newMenu -> IO ()
setFontMenu nsFontManager newMenu =
  sendMessage nsFontManager setFontMenuSelector (toNSMenu newMenu)

-- | @- fontMenu:@
fontMenu :: IsNSFontManager nsFontManager => nsFontManager -> Bool -> IO (Id NSMenu)
fontMenu nsFontManager create =
  sendMessage nsFontManager fontMenuSelector create

-- | @- fontPanel:@
fontPanel :: IsNSFontManager nsFontManager => nsFontManager -> Bool -> IO (Id NSFontPanel)
fontPanel nsFontManager create =
  sendMessage nsFontManager fontPanelSelector create

-- | @- fontWithFamily:traits:weight:size:@
fontWithFamily_traits_weight_size :: (IsNSFontManager nsFontManager, IsNSString family_) => nsFontManager -> family_ -> NSFontTraitMask -> CLong -> CDouble -> IO (Id NSFont)
fontWithFamily_traits_weight_size nsFontManager family_ traits weight size =
  sendMessage nsFontManager fontWithFamily_traits_weight_sizeSelector (toNSString family_) traits weight size

-- | @- traitsOfFont:@
traitsOfFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> IO NSFontTraitMask
traitsOfFont nsFontManager fontObj =
  sendMessage nsFontManager traitsOfFontSelector (toNSFont fontObj)

-- | @- weightOfFont:@
weightOfFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> IO CLong
weightOfFont nsFontManager fontObj =
  sendMessage nsFontManager weightOfFontSelector (toNSFont fontObj)

-- | @- availableMembersOfFontFamily:@
availableMembersOfFontFamily :: (IsNSFontManager nsFontManager, IsNSString fam) => nsFontManager -> fam -> IO (Id NSArray)
availableMembersOfFontFamily nsFontManager fam =
  sendMessage nsFontManager availableMembersOfFontFamilySelector (toNSString fam)

-- | @- convertFont:@
convertFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> IO (Id NSFont)
convertFont nsFontManager fontObj =
  sendMessage nsFontManager convertFontSelector (toNSFont fontObj)

-- | @- convertFont:toSize:@
convertFont_toSize :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> CDouble -> IO (Id NSFont)
convertFont_toSize nsFontManager fontObj size =
  sendMessage nsFontManager convertFont_toSizeSelector (toNSFont fontObj) size

-- | @- convertFont:toFace:@
convertFont_toFace :: (IsNSFontManager nsFontManager, IsNSFont fontObj, IsNSString typeface) => nsFontManager -> fontObj -> typeface -> IO (Id NSFont)
convertFont_toFace nsFontManager fontObj typeface =
  sendMessage nsFontManager convertFont_toFaceSelector (toNSFont fontObj) (toNSString typeface)

-- | @- convertFont:toFamily:@
convertFont_toFamily :: (IsNSFontManager nsFontManager, IsNSFont fontObj, IsNSString family_) => nsFontManager -> fontObj -> family_ -> IO (Id NSFont)
convertFont_toFamily nsFontManager fontObj family_ =
  sendMessage nsFontManager convertFont_toFamilySelector (toNSFont fontObj) (toNSString family_)

-- | @- convertFont:toHaveTrait:@
convertFont_toHaveTrait :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> NSFontTraitMask -> IO (Id NSFont)
convertFont_toHaveTrait nsFontManager fontObj trait =
  sendMessage nsFontManager convertFont_toHaveTraitSelector (toNSFont fontObj) trait

-- | @- convertFont:toNotHaveTrait:@
convertFont_toNotHaveTrait :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> NSFontTraitMask -> IO (Id NSFont)
convertFont_toNotHaveTrait nsFontManager fontObj trait =
  sendMessage nsFontManager convertFont_toNotHaveTraitSelector (toNSFont fontObj) trait

-- | @- convertWeight:ofFont:@
convertWeight_ofFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> Bool -> fontObj -> IO (Id NSFont)
convertWeight_ofFont nsFontManager upFlag fontObj =
  sendMessage nsFontManager convertWeight_ofFontSelector upFlag (toNSFont fontObj)

-- | @- sendAction@
sendAction :: IsNSFontManager nsFontManager => nsFontManager -> IO Bool
sendAction nsFontManager =
  sendMessage nsFontManager sendActionSelector

-- | @- localizedNameForFamily:face:@
localizedNameForFamily_face :: (IsNSFontManager nsFontManager, IsNSString family_, IsNSString faceKey) => nsFontManager -> family_ -> faceKey -> IO (Id NSString)
localizedNameForFamily_face nsFontManager family_ faceKey =
  sendMessage nsFontManager localizedNameForFamily_faceSelector (toNSString family_) (toNSString faceKey)

-- | @- setSelectedAttributes:isMultiple:@
setSelectedAttributes_isMultiple :: (IsNSFontManager nsFontManager, IsNSDictionary attributes) => nsFontManager -> attributes -> Bool -> IO ()
setSelectedAttributes_isMultiple nsFontManager attributes flag =
  sendMessage nsFontManager setSelectedAttributes_isMultipleSelector (toNSDictionary attributes) flag

-- | @- convertAttributes:@
convertAttributes :: (IsNSFontManager nsFontManager, IsNSDictionary attributes) => nsFontManager -> attributes -> IO (Id NSDictionary)
convertAttributes nsFontManager attributes =
  sendMessage nsFontManager convertAttributesSelector (toNSDictionary attributes)

-- | @- availableFontNamesMatchingFontDescriptor:@
availableFontNamesMatchingFontDescriptor :: (IsNSFontManager nsFontManager, IsNSFontDescriptor descriptor) => nsFontManager -> descriptor -> IO (Id NSArray)
availableFontNamesMatchingFontDescriptor nsFontManager descriptor =
  sendMessage nsFontManager availableFontNamesMatchingFontDescriptorSelector (toNSFontDescriptor descriptor)

-- | @- fontDescriptorsInCollection:@
fontDescriptorsInCollection :: (IsNSFontManager nsFontManager, IsNSString collectionNames) => nsFontManager -> collectionNames -> IO (Id NSArray)
fontDescriptorsInCollection nsFontManager collectionNames =
  sendMessage nsFontManager fontDescriptorsInCollectionSelector (toNSString collectionNames)

-- | @- addCollection:options:@
addCollection_options :: (IsNSFontManager nsFontManager, IsNSString collectionName) => nsFontManager -> collectionName -> NSFontCollectionOptions -> IO Bool
addCollection_options nsFontManager collectionName collectionOptions =
  sendMessage nsFontManager addCollection_optionsSelector (toNSString collectionName) collectionOptions

-- | @- removeCollection:@
removeCollection :: (IsNSFontManager nsFontManager, IsNSString collectionName) => nsFontManager -> collectionName -> IO Bool
removeCollection nsFontManager collectionName =
  sendMessage nsFontManager removeCollectionSelector (toNSString collectionName)

-- | @- addFontDescriptors:toCollection:@
addFontDescriptors_toCollection :: (IsNSFontManager nsFontManager, IsNSArray descriptors, IsNSString collectionName) => nsFontManager -> descriptors -> collectionName -> IO ()
addFontDescriptors_toCollection nsFontManager descriptors collectionName =
  sendMessage nsFontManager addFontDescriptors_toCollectionSelector (toNSArray descriptors) (toNSString collectionName)

-- | @- removeFontDescriptor:fromCollection:@
removeFontDescriptor_fromCollection :: (IsNSFontManager nsFontManager, IsNSFontDescriptor descriptor, IsNSString collection) => nsFontManager -> descriptor -> collection -> IO ()
removeFontDescriptor_fromCollection nsFontManager descriptor collection =
  sendMessage nsFontManager removeFontDescriptor_fromCollectionSelector (toNSFontDescriptor descriptor) (toNSString collection)

-- | @- convertFontTraits:@
convertFontTraits :: IsNSFontManager nsFontManager => nsFontManager -> NSFontTraitMask -> IO NSFontTraitMask
convertFontTraits nsFontManager traits =
  sendMessage nsFontManager convertFontTraitsSelector traits

-- | @- fontNamed:hasTraits:@
fontNamed_hasTraits :: (IsNSFontManager nsFontManager, IsNSString fName) => nsFontManager -> fName -> NSFontTraitMask -> IO Bool
fontNamed_hasTraits nsFontManager fName someTraits =
  sendMessage nsFontManager fontNamed_hasTraitsSelector (toNSString fName) someTraits

-- | @- availableFontNamesWithTraits:@
availableFontNamesWithTraits :: IsNSFontManager nsFontManager => nsFontManager -> NSFontTraitMask -> IO (Id NSArray)
availableFontNamesWithTraits nsFontManager someTraits =
  sendMessage nsFontManager availableFontNamesWithTraitsSelector someTraits

-- | @- addFontTrait:@
addFontTrait :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
addFontTrait nsFontManager sender =
  sendMessage nsFontManager addFontTraitSelector sender

-- | @- removeFontTrait:@
removeFontTrait :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
removeFontTrait nsFontManager sender =
  sendMessage nsFontManager removeFontTraitSelector sender

-- | @- modifyFontViaPanel:@
modifyFontViaPanel :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
modifyFontViaPanel nsFontManager sender =
  sendMessage nsFontManager modifyFontViaPanelSelector sender

-- | @- modifyFont:@
modifyFont :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
modifyFont nsFontManager sender =
  sendMessage nsFontManager modifyFontSelector sender

-- | @- orderFrontFontPanel:@
orderFrontFontPanel :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
orderFrontFontPanel nsFontManager sender =
  sendMessage nsFontManager orderFrontFontPanelSelector sender

-- | @- orderFrontStylesPanel:@
orderFrontStylesPanel :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
orderFrontStylesPanel nsFontManager sender =
  sendMessage nsFontManager orderFrontStylesPanelSelector sender

-- | @+ sharedFontManager@
sharedFontManager :: IO (Id NSFontManager)
sharedFontManager  =
  do
    cls' <- getRequiredClass "NSFontManager"
    sendClassMessage cls' sharedFontManagerSelector

-- | @- multiple@
multiple :: IsNSFontManager nsFontManager => nsFontManager -> IO Bool
multiple nsFontManager =
  sendMessage nsFontManager multipleSelector

-- | @- selectedFont@
selectedFont :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSFont)
selectedFont nsFontManager =
  sendMessage nsFontManager selectedFontSelector

-- | @- availableFonts@
availableFonts :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSArray)
availableFonts nsFontManager =
  sendMessage nsFontManager availableFontsSelector

-- | @- availableFontFamilies@
availableFontFamilies :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSArray)
availableFontFamilies nsFontManager =
  sendMessage nsFontManager availableFontFamiliesSelector

-- | @- enabled@
enabled :: IsNSFontManager nsFontManager => nsFontManager -> IO Bool
enabled nsFontManager =
  sendMessage nsFontManager enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSFontManager nsFontManager => nsFontManager -> Bool -> IO ()
setEnabled nsFontManager value =
  sendMessage nsFontManager setEnabledSelector value

-- | @- action@
action :: IsNSFontManager nsFontManager => nsFontManager -> IO Sel
action nsFontManager =
  sendMessage nsFontManager actionSelector

-- | @- setAction:@
setAction :: IsNSFontManager nsFontManager => nsFontManager -> Sel -> IO ()
setAction nsFontManager value =
  sendMessage nsFontManager setActionSelector value

-- | @- delegate@
delegate :: IsNSFontManager nsFontManager => nsFontManager -> IO RawId
delegate nsFontManager =
  sendMessage nsFontManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
setDelegate nsFontManager value =
  sendMessage nsFontManager setDelegateSelector value

-- | @- collectionNames@
collectionNames :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSArray)
collectionNames nsFontManager =
  sendMessage nsFontManager collectionNamesSelector

-- | @- currentFontAction@
currentFontAction :: IsNSFontManager nsFontManager => nsFontManager -> IO NSFontAction
currentFontAction nsFontManager =
  sendMessage nsFontManager currentFontActionSelector

-- | @- target@
target :: IsNSFontManager nsFontManager => nsFontManager -> IO RawId
target nsFontManager =
  sendMessage nsFontManager targetSelector

-- | @- setTarget:@
setTarget :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
setTarget nsFontManager value =
  sendMessage nsFontManager setTargetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFontPanelFactory:@
setFontPanelFactorySelector :: Selector '[Class] ()
setFontPanelFactorySelector = mkSelector "setFontPanelFactory:"

-- | @Selector@ for @setFontManagerFactory:@
setFontManagerFactorySelector :: Selector '[Class] ()
setFontManagerFactorySelector = mkSelector "setFontManagerFactory:"

-- | @Selector@ for @setSelectedFont:isMultiple:@
setSelectedFont_isMultipleSelector :: Selector '[Id NSFont, Bool] ()
setSelectedFont_isMultipleSelector = mkSelector "setSelectedFont:isMultiple:"

-- | @Selector@ for @setFontMenu:@
setFontMenuSelector :: Selector '[Id NSMenu] ()
setFontMenuSelector = mkSelector "setFontMenu:"

-- | @Selector@ for @fontMenu:@
fontMenuSelector :: Selector '[Bool] (Id NSMenu)
fontMenuSelector = mkSelector "fontMenu:"

-- | @Selector@ for @fontPanel:@
fontPanelSelector :: Selector '[Bool] (Id NSFontPanel)
fontPanelSelector = mkSelector "fontPanel:"

-- | @Selector@ for @fontWithFamily:traits:weight:size:@
fontWithFamily_traits_weight_sizeSelector :: Selector '[Id NSString, NSFontTraitMask, CLong, CDouble] (Id NSFont)
fontWithFamily_traits_weight_sizeSelector = mkSelector "fontWithFamily:traits:weight:size:"

-- | @Selector@ for @traitsOfFont:@
traitsOfFontSelector :: Selector '[Id NSFont] NSFontTraitMask
traitsOfFontSelector = mkSelector "traitsOfFont:"

-- | @Selector@ for @weightOfFont:@
weightOfFontSelector :: Selector '[Id NSFont] CLong
weightOfFontSelector = mkSelector "weightOfFont:"

-- | @Selector@ for @availableMembersOfFontFamily:@
availableMembersOfFontFamilySelector :: Selector '[Id NSString] (Id NSArray)
availableMembersOfFontFamilySelector = mkSelector "availableMembersOfFontFamily:"

-- | @Selector@ for @convertFont:@
convertFontSelector :: Selector '[Id NSFont] (Id NSFont)
convertFontSelector = mkSelector "convertFont:"

-- | @Selector@ for @convertFont:toSize:@
convertFont_toSizeSelector :: Selector '[Id NSFont, CDouble] (Id NSFont)
convertFont_toSizeSelector = mkSelector "convertFont:toSize:"

-- | @Selector@ for @convertFont:toFace:@
convertFont_toFaceSelector :: Selector '[Id NSFont, Id NSString] (Id NSFont)
convertFont_toFaceSelector = mkSelector "convertFont:toFace:"

-- | @Selector@ for @convertFont:toFamily:@
convertFont_toFamilySelector :: Selector '[Id NSFont, Id NSString] (Id NSFont)
convertFont_toFamilySelector = mkSelector "convertFont:toFamily:"

-- | @Selector@ for @convertFont:toHaveTrait:@
convertFont_toHaveTraitSelector :: Selector '[Id NSFont, NSFontTraitMask] (Id NSFont)
convertFont_toHaveTraitSelector = mkSelector "convertFont:toHaveTrait:"

-- | @Selector@ for @convertFont:toNotHaveTrait:@
convertFont_toNotHaveTraitSelector :: Selector '[Id NSFont, NSFontTraitMask] (Id NSFont)
convertFont_toNotHaveTraitSelector = mkSelector "convertFont:toNotHaveTrait:"

-- | @Selector@ for @convertWeight:ofFont:@
convertWeight_ofFontSelector :: Selector '[Bool, Id NSFont] (Id NSFont)
convertWeight_ofFontSelector = mkSelector "convertWeight:ofFont:"

-- | @Selector@ for @sendAction@
sendActionSelector :: Selector '[] Bool
sendActionSelector = mkSelector "sendAction"

-- | @Selector@ for @localizedNameForFamily:face:@
localizedNameForFamily_faceSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
localizedNameForFamily_faceSelector = mkSelector "localizedNameForFamily:face:"

-- | @Selector@ for @setSelectedAttributes:isMultiple:@
setSelectedAttributes_isMultipleSelector :: Selector '[Id NSDictionary, Bool] ()
setSelectedAttributes_isMultipleSelector = mkSelector "setSelectedAttributes:isMultiple:"

-- | @Selector@ for @convertAttributes:@
convertAttributesSelector :: Selector '[Id NSDictionary] (Id NSDictionary)
convertAttributesSelector = mkSelector "convertAttributes:"

-- | @Selector@ for @availableFontNamesMatchingFontDescriptor:@
availableFontNamesMatchingFontDescriptorSelector :: Selector '[Id NSFontDescriptor] (Id NSArray)
availableFontNamesMatchingFontDescriptorSelector = mkSelector "availableFontNamesMatchingFontDescriptor:"

-- | @Selector@ for @fontDescriptorsInCollection:@
fontDescriptorsInCollectionSelector :: Selector '[Id NSString] (Id NSArray)
fontDescriptorsInCollectionSelector = mkSelector "fontDescriptorsInCollection:"

-- | @Selector@ for @addCollection:options:@
addCollection_optionsSelector :: Selector '[Id NSString, NSFontCollectionOptions] Bool
addCollection_optionsSelector = mkSelector "addCollection:options:"

-- | @Selector@ for @removeCollection:@
removeCollectionSelector :: Selector '[Id NSString] Bool
removeCollectionSelector = mkSelector "removeCollection:"

-- | @Selector@ for @addFontDescriptors:toCollection:@
addFontDescriptors_toCollectionSelector :: Selector '[Id NSArray, Id NSString] ()
addFontDescriptors_toCollectionSelector = mkSelector "addFontDescriptors:toCollection:"

-- | @Selector@ for @removeFontDescriptor:fromCollection:@
removeFontDescriptor_fromCollectionSelector :: Selector '[Id NSFontDescriptor, Id NSString] ()
removeFontDescriptor_fromCollectionSelector = mkSelector "removeFontDescriptor:fromCollection:"

-- | @Selector@ for @convertFontTraits:@
convertFontTraitsSelector :: Selector '[NSFontTraitMask] NSFontTraitMask
convertFontTraitsSelector = mkSelector "convertFontTraits:"

-- | @Selector@ for @fontNamed:hasTraits:@
fontNamed_hasTraitsSelector :: Selector '[Id NSString, NSFontTraitMask] Bool
fontNamed_hasTraitsSelector = mkSelector "fontNamed:hasTraits:"

-- | @Selector@ for @availableFontNamesWithTraits:@
availableFontNamesWithTraitsSelector :: Selector '[NSFontTraitMask] (Id NSArray)
availableFontNamesWithTraitsSelector = mkSelector "availableFontNamesWithTraits:"

-- | @Selector@ for @addFontTrait:@
addFontTraitSelector :: Selector '[RawId] ()
addFontTraitSelector = mkSelector "addFontTrait:"

-- | @Selector@ for @removeFontTrait:@
removeFontTraitSelector :: Selector '[RawId] ()
removeFontTraitSelector = mkSelector "removeFontTrait:"

-- | @Selector@ for @modifyFontViaPanel:@
modifyFontViaPanelSelector :: Selector '[RawId] ()
modifyFontViaPanelSelector = mkSelector "modifyFontViaPanel:"

-- | @Selector@ for @modifyFont:@
modifyFontSelector :: Selector '[RawId] ()
modifyFontSelector = mkSelector "modifyFont:"

-- | @Selector@ for @orderFrontFontPanel:@
orderFrontFontPanelSelector :: Selector '[RawId] ()
orderFrontFontPanelSelector = mkSelector "orderFrontFontPanel:"

-- | @Selector@ for @orderFrontStylesPanel:@
orderFrontStylesPanelSelector :: Selector '[RawId] ()
orderFrontStylesPanelSelector = mkSelector "orderFrontStylesPanel:"

-- | @Selector@ for @sharedFontManager@
sharedFontManagerSelector :: Selector '[] (Id NSFontManager)
sharedFontManagerSelector = mkSelector "sharedFontManager"

-- | @Selector@ for @multiple@
multipleSelector :: Selector '[] Bool
multipleSelector = mkSelector "multiple"

-- | @Selector@ for @selectedFont@
selectedFontSelector :: Selector '[] (Id NSFont)
selectedFontSelector = mkSelector "selectedFont"

-- | @Selector@ for @availableFonts@
availableFontsSelector :: Selector '[] (Id NSArray)
availableFontsSelector = mkSelector "availableFonts"

-- | @Selector@ for @availableFontFamilies@
availableFontFamiliesSelector :: Selector '[] (Id NSArray)
availableFontFamiliesSelector = mkSelector "availableFontFamilies"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @collectionNames@
collectionNamesSelector :: Selector '[] (Id NSArray)
collectionNamesSelector = mkSelector "collectionNames"

-- | @Selector@ for @currentFontAction@
currentFontActionSelector :: Selector '[] NSFontAction
currentFontActionSelector = mkSelector "currentFontAction"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

