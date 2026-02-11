{-# LANGUAGE PatternSynonyms #-}
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
  , setFontPanelFactorySelector
  , setFontManagerFactorySelector
  , setSelectedFont_isMultipleSelector
  , setFontMenuSelector
  , fontMenuSelector
  , fontPanelSelector
  , fontWithFamily_traits_weight_sizeSelector
  , traitsOfFontSelector
  , weightOfFontSelector
  , availableMembersOfFontFamilySelector
  , convertFontSelector
  , convertFont_toSizeSelector
  , convertFont_toFaceSelector
  , convertFont_toFamilySelector
  , convertFont_toHaveTraitSelector
  , convertFont_toNotHaveTraitSelector
  , convertWeight_ofFontSelector
  , sendActionSelector
  , localizedNameForFamily_faceSelector
  , setSelectedAttributes_isMultipleSelector
  , convertAttributesSelector
  , availableFontNamesMatchingFontDescriptorSelector
  , fontDescriptorsInCollectionSelector
  , addCollection_optionsSelector
  , removeCollectionSelector
  , addFontDescriptors_toCollectionSelector
  , removeFontDescriptor_fromCollectionSelector
  , convertFontTraitsSelector
  , fontNamed_hasTraitsSelector
  , availableFontNamesWithTraitsSelector
  , addFontTraitSelector
  , removeFontTraitSelector
  , modifyFontViaPanelSelector
  , modifyFontSelector
  , orderFrontFontPanelSelector
  , orderFrontStylesPanelSelector
  , sharedFontManagerSelector
  , multipleSelector
  , selectedFontSelector
  , availableFontsSelector
  , availableFontFamiliesSelector
  , enabledSelector
  , setEnabledSelector
  , actionSelector
  , setActionSelector
  , delegateSelector
  , setDelegateSelector
  , collectionNamesSelector
  , currentFontActionSelector
  , targetSelector
  , setTargetSelector

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

-- | @+ setFontPanelFactory:@
setFontPanelFactory :: Class -> IO ()
setFontPanelFactory factoryId =
  do
    cls' <- getRequiredClass "NSFontManager"
    sendClassMsg cls' (mkSelector "setFontPanelFactory:") retVoid [argPtr (unClass factoryId)]

-- | @+ setFontManagerFactory:@
setFontManagerFactory :: Class -> IO ()
setFontManagerFactory factoryId =
  do
    cls' <- getRequiredClass "NSFontManager"
    sendClassMsg cls' (mkSelector "setFontManagerFactory:") retVoid [argPtr (unClass factoryId)]

-- | @- setSelectedFont:isMultiple:@
setSelectedFont_isMultiple :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> Bool -> IO ()
setSelectedFont_isMultiple nsFontManager  fontObj flag =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "setSelectedFont:isMultiple:") retVoid [argPtr (castPtr raw_fontObj :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- setFontMenu:@
setFontMenu :: (IsNSFontManager nsFontManager, IsNSMenu newMenu) => nsFontManager -> newMenu -> IO ()
setFontMenu nsFontManager  newMenu =
  withObjCPtr newMenu $ \raw_newMenu ->
      sendMsg nsFontManager (mkSelector "setFontMenu:") retVoid [argPtr (castPtr raw_newMenu :: Ptr ())]

-- | @- fontMenu:@
fontMenu :: IsNSFontManager nsFontManager => nsFontManager -> Bool -> IO (Id NSMenu)
fontMenu nsFontManager  create =
    sendMsg nsFontManager (mkSelector "fontMenu:") (retPtr retVoid) [argCULong (if create then 1 else 0)] >>= retainedObject . castPtr

-- | @- fontPanel:@
fontPanel :: IsNSFontManager nsFontManager => nsFontManager -> Bool -> IO (Id NSFontPanel)
fontPanel nsFontManager  create =
    sendMsg nsFontManager (mkSelector "fontPanel:") (retPtr retVoid) [argCULong (if create then 1 else 0)] >>= retainedObject . castPtr

-- | @- fontWithFamily:traits:weight:size:@
fontWithFamily_traits_weight_size :: (IsNSFontManager nsFontManager, IsNSString family_) => nsFontManager -> family_ -> NSFontTraitMask -> CLong -> CDouble -> IO (Id NSFont)
fontWithFamily_traits_weight_size nsFontManager  family_ traits weight size =
  withObjCPtr family_ $ \raw_family_ ->
      sendMsg nsFontManager (mkSelector "fontWithFamily:traits:weight:size:") (retPtr retVoid) [argPtr (castPtr raw_family_ :: Ptr ()), argCULong (coerce traits), argCLong weight, argCDouble size] >>= retainedObject . castPtr

-- | @- traitsOfFont:@
traitsOfFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> IO NSFontTraitMask
traitsOfFont nsFontManager  fontObj =
  withObjCPtr fontObj $ \raw_fontObj ->
      fmap (coerce :: CULong -> NSFontTraitMask) $ sendMsg nsFontManager (mkSelector "traitsOfFont:") retCULong [argPtr (castPtr raw_fontObj :: Ptr ())]

-- | @- weightOfFont:@
weightOfFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> IO CLong
weightOfFont nsFontManager  fontObj =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "weightOfFont:") retCLong [argPtr (castPtr raw_fontObj :: Ptr ())]

-- | @- availableMembersOfFontFamily:@
availableMembersOfFontFamily :: (IsNSFontManager nsFontManager, IsNSString fam) => nsFontManager -> fam -> IO (Id NSArray)
availableMembersOfFontFamily nsFontManager  fam =
  withObjCPtr fam $ \raw_fam ->
      sendMsg nsFontManager (mkSelector "availableMembersOfFontFamily:") (retPtr retVoid) [argPtr (castPtr raw_fam :: Ptr ())] >>= retainedObject . castPtr

-- | @- convertFont:@
convertFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> IO (Id NSFont)
convertFont nsFontManager  fontObj =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "convertFont:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ())] >>= retainedObject . castPtr

-- | @- convertFont:toSize:@
convertFont_toSize :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> CDouble -> IO (Id NSFont)
convertFont_toSize nsFontManager  fontObj size =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "convertFont:toSize:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ()), argCDouble size] >>= retainedObject . castPtr

-- | @- convertFont:toFace:@
convertFont_toFace :: (IsNSFontManager nsFontManager, IsNSFont fontObj, IsNSString typeface) => nsFontManager -> fontObj -> typeface -> IO (Id NSFont)
convertFont_toFace nsFontManager  fontObj typeface =
  withObjCPtr fontObj $ \raw_fontObj ->
    withObjCPtr typeface $ \raw_typeface ->
        sendMsg nsFontManager (mkSelector "convertFont:toFace:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ()), argPtr (castPtr raw_typeface :: Ptr ())] >>= retainedObject . castPtr

-- | @- convertFont:toFamily:@
convertFont_toFamily :: (IsNSFontManager nsFontManager, IsNSFont fontObj, IsNSString family_) => nsFontManager -> fontObj -> family_ -> IO (Id NSFont)
convertFont_toFamily nsFontManager  fontObj family_ =
  withObjCPtr fontObj $ \raw_fontObj ->
    withObjCPtr family_ $ \raw_family_ ->
        sendMsg nsFontManager (mkSelector "convertFont:toFamily:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ()), argPtr (castPtr raw_family_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- convertFont:toHaveTrait:@
convertFont_toHaveTrait :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> NSFontTraitMask -> IO (Id NSFont)
convertFont_toHaveTrait nsFontManager  fontObj trait =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "convertFont:toHaveTrait:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ()), argCULong (coerce trait)] >>= retainedObject . castPtr

-- | @- convertFont:toNotHaveTrait:@
convertFont_toNotHaveTrait :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> fontObj -> NSFontTraitMask -> IO (Id NSFont)
convertFont_toNotHaveTrait nsFontManager  fontObj trait =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "convertFont:toNotHaveTrait:") (retPtr retVoid) [argPtr (castPtr raw_fontObj :: Ptr ()), argCULong (coerce trait)] >>= retainedObject . castPtr

-- | @- convertWeight:ofFont:@
convertWeight_ofFont :: (IsNSFontManager nsFontManager, IsNSFont fontObj) => nsFontManager -> Bool -> fontObj -> IO (Id NSFont)
convertWeight_ofFont nsFontManager  upFlag fontObj =
  withObjCPtr fontObj $ \raw_fontObj ->
      sendMsg nsFontManager (mkSelector "convertWeight:ofFont:") (retPtr retVoid) [argCULong (if upFlag then 1 else 0), argPtr (castPtr raw_fontObj :: Ptr ())] >>= retainedObject . castPtr

-- | @- sendAction@
sendAction :: IsNSFontManager nsFontManager => nsFontManager -> IO Bool
sendAction nsFontManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontManager (mkSelector "sendAction") retCULong []

-- | @- localizedNameForFamily:face:@
localizedNameForFamily_face :: (IsNSFontManager nsFontManager, IsNSString family_, IsNSString faceKey) => nsFontManager -> family_ -> faceKey -> IO (Id NSString)
localizedNameForFamily_face nsFontManager  family_ faceKey =
  withObjCPtr family_ $ \raw_family_ ->
    withObjCPtr faceKey $ \raw_faceKey ->
        sendMsg nsFontManager (mkSelector "localizedNameForFamily:face:") (retPtr retVoid) [argPtr (castPtr raw_family_ :: Ptr ()), argPtr (castPtr raw_faceKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- setSelectedAttributes:isMultiple:@
setSelectedAttributes_isMultiple :: (IsNSFontManager nsFontManager, IsNSDictionary attributes) => nsFontManager -> attributes -> Bool -> IO ()
setSelectedAttributes_isMultiple nsFontManager  attributes flag =
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg nsFontManager (mkSelector "setSelectedAttributes:isMultiple:") retVoid [argPtr (castPtr raw_attributes :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- convertAttributes:@
convertAttributes :: (IsNSFontManager nsFontManager, IsNSDictionary attributes) => nsFontManager -> attributes -> IO (Id NSDictionary)
convertAttributes nsFontManager  attributes =
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg nsFontManager (mkSelector "convertAttributes:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @- availableFontNamesMatchingFontDescriptor:@
availableFontNamesMatchingFontDescriptor :: (IsNSFontManager nsFontManager, IsNSFontDescriptor descriptor) => nsFontManager -> descriptor -> IO (Id NSArray)
availableFontNamesMatchingFontDescriptor nsFontManager  descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg nsFontManager (mkSelector "availableFontNamesMatchingFontDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- fontDescriptorsInCollection:@
fontDescriptorsInCollection :: (IsNSFontManager nsFontManager, IsNSString collectionNames) => nsFontManager -> collectionNames -> IO (Id NSArray)
fontDescriptorsInCollection nsFontManager  collectionNames =
  withObjCPtr collectionNames $ \raw_collectionNames ->
      sendMsg nsFontManager (mkSelector "fontDescriptorsInCollection:") (retPtr retVoid) [argPtr (castPtr raw_collectionNames :: Ptr ())] >>= retainedObject . castPtr

-- | @- addCollection:options:@
addCollection_options :: (IsNSFontManager nsFontManager, IsNSString collectionName) => nsFontManager -> collectionName -> NSFontCollectionOptions -> IO Bool
addCollection_options nsFontManager  collectionName collectionOptions =
  withObjCPtr collectionName $ \raw_collectionName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontManager (mkSelector "addCollection:options:") retCULong [argPtr (castPtr raw_collectionName :: Ptr ()), argCULong (coerce collectionOptions)]

-- | @- removeCollection:@
removeCollection :: (IsNSFontManager nsFontManager, IsNSString collectionName) => nsFontManager -> collectionName -> IO Bool
removeCollection nsFontManager  collectionName =
  withObjCPtr collectionName $ \raw_collectionName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontManager (mkSelector "removeCollection:") retCULong [argPtr (castPtr raw_collectionName :: Ptr ())]

-- | @- addFontDescriptors:toCollection:@
addFontDescriptors_toCollection :: (IsNSFontManager nsFontManager, IsNSArray descriptors, IsNSString collectionName) => nsFontManager -> descriptors -> collectionName -> IO ()
addFontDescriptors_toCollection nsFontManager  descriptors collectionName =
  withObjCPtr descriptors $ \raw_descriptors ->
    withObjCPtr collectionName $ \raw_collectionName ->
        sendMsg nsFontManager (mkSelector "addFontDescriptors:toCollection:") retVoid [argPtr (castPtr raw_descriptors :: Ptr ()), argPtr (castPtr raw_collectionName :: Ptr ())]

-- | @- removeFontDescriptor:fromCollection:@
removeFontDescriptor_fromCollection :: (IsNSFontManager nsFontManager, IsNSFontDescriptor descriptor, IsNSString collection) => nsFontManager -> descriptor -> collection -> IO ()
removeFontDescriptor_fromCollection nsFontManager  descriptor collection =
  withObjCPtr descriptor $ \raw_descriptor ->
    withObjCPtr collection $ \raw_collection ->
        sendMsg nsFontManager (mkSelector "removeFontDescriptor:fromCollection:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_collection :: Ptr ())]

-- | @- convertFontTraits:@
convertFontTraits :: IsNSFontManager nsFontManager => nsFontManager -> NSFontTraitMask -> IO NSFontTraitMask
convertFontTraits nsFontManager  traits =
    fmap (coerce :: CULong -> NSFontTraitMask) $ sendMsg nsFontManager (mkSelector "convertFontTraits:") retCULong [argCULong (coerce traits)]

-- | @- fontNamed:hasTraits:@
fontNamed_hasTraits :: (IsNSFontManager nsFontManager, IsNSString fName) => nsFontManager -> fName -> NSFontTraitMask -> IO Bool
fontNamed_hasTraits nsFontManager  fName someTraits =
  withObjCPtr fName $ \raw_fName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontManager (mkSelector "fontNamed:hasTraits:") retCULong [argPtr (castPtr raw_fName :: Ptr ()), argCULong (coerce someTraits)]

-- | @- availableFontNamesWithTraits:@
availableFontNamesWithTraits :: IsNSFontManager nsFontManager => nsFontManager -> NSFontTraitMask -> IO (Id NSArray)
availableFontNamesWithTraits nsFontManager  someTraits =
    sendMsg nsFontManager (mkSelector "availableFontNamesWithTraits:") (retPtr retVoid) [argCULong (coerce someTraits)] >>= retainedObject . castPtr

-- | @- addFontTrait:@
addFontTrait :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
addFontTrait nsFontManager  sender =
    sendMsg nsFontManager (mkSelector "addFontTrait:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- removeFontTrait:@
removeFontTrait :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
removeFontTrait nsFontManager  sender =
    sendMsg nsFontManager (mkSelector "removeFontTrait:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- modifyFontViaPanel:@
modifyFontViaPanel :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
modifyFontViaPanel nsFontManager  sender =
    sendMsg nsFontManager (mkSelector "modifyFontViaPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- modifyFont:@
modifyFont :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
modifyFont nsFontManager  sender =
    sendMsg nsFontManager (mkSelector "modifyFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontFontPanel:@
orderFrontFontPanel :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
orderFrontFontPanel nsFontManager  sender =
    sendMsg nsFontManager (mkSelector "orderFrontFontPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontStylesPanel:@
orderFrontStylesPanel :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
orderFrontStylesPanel nsFontManager  sender =
    sendMsg nsFontManager (mkSelector "orderFrontStylesPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @+ sharedFontManager@
sharedFontManager :: IO (Id NSFontManager)
sharedFontManager  =
  do
    cls' <- getRequiredClass "NSFontManager"
    sendClassMsg cls' (mkSelector "sharedFontManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- multiple@
multiple :: IsNSFontManager nsFontManager => nsFontManager -> IO Bool
multiple nsFontManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontManager (mkSelector "multiple") retCULong []

-- | @- selectedFont@
selectedFont :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSFont)
selectedFont nsFontManager  =
    sendMsg nsFontManager (mkSelector "selectedFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- availableFonts@
availableFonts :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSArray)
availableFonts nsFontManager  =
    sendMsg nsFontManager (mkSelector "availableFonts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- availableFontFamilies@
availableFontFamilies :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSArray)
availableFontFamilies nsFontManager  =
    sendMsg nsFontManager (mkSelector "availableFontFamilies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enabled@
enabled :: IsNSFontManager nsFontManager => nsFontManager -> IO Bool
enabled nsFontManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontManager (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSFontManager nsFontManager => nsFontManager -> Bool -> IO ()
setEnabled nsFontManager  value =
    sendMsg nsFontManager (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- action@
action :: IsNSFontManager nsFontManager => nsFontManager -> IO Selector
action nsFontManager  =
    fmap (Selector . castPtr) $ sendMsg nsFontManager (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSFontManager nsFontManager => nsFontManager -> Selector -> IO ()
setAction nsFontManager  value =
    sendMsg nsFontManager (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- delegate@
delegate :: IsNSFontManager nsFontManager => nsFontManager -> IO RawId
delegate nsFontManager  =
    fmap (RawId . castPtr) $ sendMsg nsFontManager (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
setDelegate nsFontManager  value =
    sendMsg nsFontManager (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- collectionNames@
collectionNames :: IsNSFontManager nsFontManager => nsFontManager -> IO (Id NSArray)
collectionNames nsFontManager  =
    sendMsg nsFontManager (mkSelector "collectionNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentFontAction@
currentFontAction :: IsNSFontManager nsFontManager => nsFontManager -> IO NSFontAction
currentFontAction nsFontManager  =
    fmap (coerce :: CULong -> NSFontAction) $ sendMsg nsFontManager (mkSelector "currentFontAction") retCULong []

-- | @- target@
target :: IsNSFontManager nsFontManager => nsFontManager -> IO RawId
target nsFontManager  =
    fmap (RawId . castPtr) $ sendMsg nsFontManager (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSFontManager nsFontManager => nsFontManager -> RawId -> IO ()
setTarget nsFontManager  value =
    sendMsg nsFontManager (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFontPanelFactory:@
setFontPanelFactorySelector :: Selector
setFontPanelFactorySelector = mkSelector "setFontPanelFactory:"

-- | @Selector@ for @setFontManagerFactory:@
setFontManagerFactorySelector :: Selector
setFontManagerFactorySelector = mkSelector "setFontManagerFactory:"

-- | @Selector@ for @setSelectedFont:isMultiple:@
setSelectedFont_isMultipleSelector :: Selector
setSelectedFont_isMultipleSelector = mkSelector "setSelectedFont:isMultiple:"

-- | @Selector@ for @setFontMenu:@
setFontMenuSelector :: Selector
setFontMenuSelector = mkSelector "setFontMenu:"

-- | @Selector@ for @fontMenu:@
fontMenuSelector :: Selector
fontMenuSelector = mkSelector "fontMenu:"

-- | @Selector@ for @fontPanel:@
fontPanelSelector :: Selector
fontPanelSelector = mkSelector "fontPanel:"

-- | @Selector@ for @fontWithFamily:traits:weight:size:@
fontWithFamily_traits_weight_sizeSelector :: Selector
fontWithFamily_traits_weight_sizeSelector = mkSelector "fontWithFamily:traits:weight:size:"

-- | @Selector@ for @traitsOfFont:@
traitsOfFontSelector :: Selector
traitsOfFontSelector = mkSelector "traitsOfFont:"

-- | @Selector@ for @weightOfFont:@
weightOfFontSelector :: Selector
weightOfFontSelector = mkSelector "weightOfFont:"

-- | @Selector@ for @availableMembersOfFontFamily:@
availableMembersOfFontFamilySelector :: Selector
availableMembersOfFontFamilySelector = mkSelector "availableMembersOfFontFamily:"

-- | @Selector@ for @convertFont:@
convertFontSelector :: Selector
convertFontSelector = mkSelector "convertFont:"

-- | @Selector@ for @convertFont:toSize:@
convertFont_toSizeSelector :: Selector
convertFont_toSizeSelector = mkSelector "convertFont:toSize:"

-- | @Selector@ for @convertFont:toFace:@
convertFont_toFaceSelector :: Selector
convertFont_toFaceSelector = mkSelector "convertFont:toFace:"

-- | @Selector@ for @convertFont:toFamily:@
convertFont_toFamilySelector :: Selector
convertFont_toFamilySelector = mkSelector "convertFont:toFamily:"

-- | @Selector@ for @convertFont:toHaveTrait:@
convertFont_toHaveTraitSelector :: Selector
convertFont_toHaveTraitSelector = mkSelector "convertFont:toHaveTrait:"

-- | @Selector@ for @convertFont:toNotHaveTrait:@
convertFont_toNotHaveTraitSelector :: Selector
convertFont_toNotHaveTraitSelector = mkSelector "convertFont:toNotHaveTrait:"

-- | @Selector@ for @convertWeight:ofFont:@
convertWeight_ofFontSelector :: Selector
convertWeight_ofFontSelector = mkSelector "convertWeight:ofFont:"

-- | @Selector@ for @sendAction@
sendActionSelector :: Selector
sendActionSelector = mkSelector "sendAction"

-- | @Selector@ for @localizedNameForFamily:face:@
localizedNameForFamily_faceSelector :: Selector
localizedNameForFamily_faceSelector = mkSelector "localizedNameForFamily:face:"

-- | @Selector@ for @setSelectedAttributes:isMultiple:@
setSelectedAttributes_isMultipleSelector :: Selector
setSelectedAttributes_isMultipleSelector = mkSelector "setSelectedAttributes:isMultiple:"

-- | @Selector@ for @convertAttributes:@
convertAttributesSelector :: Selector
convertAttributesSelector = mkSelector "convertAttributes:"

-- | @Selector@ for @availableFontNamesMatchingFontDescriptor:@
availableFontNamesMatchingFontDescriptorSelector :: Selector
availableFontNamesMatchingFontDescriptorSelector = mkSelector "availableFontNamesMatchingFontDescriptor:"

-- | @Selector@ for @fontDescriptorsInCollection:@
fontDescriptorsInCollectionSelector :: Selector
fontDescriptorsInCollectionSelector = mkSelector "fontDescriptorsInCollection:"

-- | @Selector@ for @addCollection:options:@
addCollection_optionsSelector :: Selector
addCollection_optionsSelector = mkSelector "addCollection:options:"

-- | @Selector@ for @removeCollection:@
removeCollectionSelector :: Selector
removeCollectionSelector = mkSelector "removeCollection:"

-- | @Selector@ for @addFontDescriptors:toCollection:@
addFontDescriptors_toCollectionSelector :: Selector
addFontDescriptors_toCollectionSelector = mkSelector "addFontDescriptors:toCollection:"

-- | @Selector@ for @removeFontDescriptor:fromCollection:@
removeFontDescriptor_fromCollectionSelector :: Selector
removeFontDescriptor_fromCollectionSelector = mkSelector "removeFontDescriptor:fromCollection:"

-- | @Selector@ for @convertFontTraits:@
convertFontTraitsSelector :: Selector
convertFontTraitsSelector = mkSelector "convertFontTraits:"

-- | @Selector@ for @fontNamed:hasTraits:@
fontNamed_hasTraitsSelector :: Selector
fontNamed_hasTraitsSelector = mkSelector "fontNamed:hasTraits:"

-- | @Selector@ for @availableFontNamesWithTraits:@
availableFontNamesWithTraitsSelector :: Selector
availableFontNamesWithTraitsSelector = mkSelector "availableFontNamesWithTraits:"

-- | @Selector@ for @addFontTrait:@
addFontTraitSelector :: Selector
addFontTraitSelector = mkSelector "addFontTrait:"

-- | @Selector@ for @removeFontTrait:@
removeFontTraitSelector :: Selector
removeFontTraitSelector = mkSelector "removeFontTrait:"

-- | @Selector@ for @modifyFontViaPanel:@
modifyFontViaPanelSelector :: Selector
modifyFontViaPanelSelector = mkSelector "modifyFontViaPanel:"

-- | @Selector@ for @modifyFont:@
modifyFontSelector :: Selector
modifyFontSelector = mkSelector "modifyFont:"

-- | @Selector@ for @orderFrontFontPanel:@
orderFrontFontPanelSelector :: Selector
orderFrontFontPanelSelector = mkSelector "orderFrontFontPanel:"

-- | @Selector@ for @orderFrontStylesPanel:@
orderFrontStylesPanelSelector :: Selector
orderFrontStylesPanelSelector = mkSelector "orderFrontStylesPanel:"

-- | @Selector@ for @sharedFontManager@
sharedFontManagerSelector :: Selector
sharedFontManagerSelector = mkSelector "sharedFontManager"

-- | @Selector@ for @multiple@
multipleSelector :: Selector
multipleSelector = mkSelector "multiple"

-- | @Selector@ for @selectedFont@
selectedFontSelector :: Selector
selectedFontSelector = mkSelector "selectedFont"

-- | @Selector@ for @availableFonts@
availableFontsSelector :: Selector
availableFontsSelector = mkSelector "availableFonts"

-- | @Selector@ for @availableFontFamilies@
availableFontFamiliesSelector :: Selector
availableFontFamiliesSelector = mkSelector "availableFontFamilies"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @collectionNames@
collectionNamesSelector :: Selector
collectionNamesSelector = mkSelector "collectionNames"

-- | @Selector@ for @currentFontAction@
currentFontActionSelector :: Selector
currentFontActionSelector = mkSelector "currentFontAction"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

