{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFontDescriptor@.
module ObjC.AppKit.NSFontDescriptor
  ( NSFontDescriptor
  , IsNSFontDescriptor(..)
  , objectForKey
  , fontDescriptorWithFontAttributes
  , fontDescriptorWithName_size
  , fontDescriptorWithName_matrix
  , initWithFontAttributes
  , matchingFontDescriptorsWithMandatoryKeys
  , matchingFontDescriptorWithMandatoryKeys
  , fontDescriptorByAddingAttributes
  , fontDescriptorWithSymbolicTraits
  , fontDescriptorWithSize
  , fontDescriptorWithMatrix
  , fontDescriptorWithFace
  , fontDescriptorWithFamily
  , fontDescriptorWithDesign
  , preferredFontDescriptorForTextStyle_options
  , postscriptName
  , pointSize
  , matrix
  , symbolicTraits
  , requiresFontAssetRequest
  , fontAttributes
  , fontAttributesSelector
  , fontDescriptorByAddingAttributesSelector
  , fontDescriptorWithDesignSelector
  , fontDescriptorWithFaceSelector
  , fontDescriptorWithFamilySelector
  , fontDescriptorWithFontAttributesSelector
  , fontDescriptorWithMatrixSelector
  , fontDescriptorWithName_matrixSelector
  , fontDescriptorWithName_sizeSelector
  , fontDescriptorWithSizeSelector
  , fontDescriptorWithSymbolicTraitsSelector
  , initWithFontAttributesSelector
  , matchingFontDescriptorWithMandatoryKeysSelector
  , matchingFontDescriptorsWithMandatoryKeysSelector
  , matrixSelector
  , objectForKeySelector
  , pointSizeSelector
  , postscriptNameSelector
  , preferredFontDescriptorForTextStyle_optionsSelector
  , requiresFontAssetRequestSelector
  , symbolicTraitsSelector

  -- * Enum types
  , NSFontDescriptorSymbolicTraits(NSFontDescriptorSymbolicTraits)
  , pattern NSFontDescriptorTraitItalic
  , pattern NSFontDescriptorTraitBold
  , pattern NSFontDescriptorTraitExpanded
  , pattern NSFontDescriptorTraitCondensed
  , pattern NSFontDescriptorTraitMonoSpace
  , pattern NSFontDescriptorTraitVertical
  , pattern NSFontDescriptorTraitUIOptimized
  , pattern NSFontDescriptorTraitTightLeading
  , pattern NSFontDescriptorTraitLooseLeading
  , pattern NSFontDescriptorTraitEmphasized
  , pattern NSFontDescriptorClassMask
  , pattern NSFontDescriptorClassUnknown
  , pattern NSFontDescriptorClassOldStyleSerifs
  , pattern NSFontDescriptorClassTransitionalSerifs
  , pattern NSFontDescriptorClassModernSerifs
  , pattern NSFontDescriptorClassClarendonSerifs
  , pattern NSFontDescriptorClassSlabSerifs
  , pattern NSFontDescriptorClassFreeformSerifs
  , pattern NSFontDescriptorClassSansSerif
  , pattern NSFontDescriptorClassOrnamentals
  , pattern NSFontDescriptorClassScripts
  , pattern NSFontDescriptorClassSymbolic

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

-- | @- objectForKey:@
objectForKey :: (IsNSFontDescriptor nsFontDescriptor, IsNSString attribute) => nsFontDescriptor -> attribute -> IO RawId
objectForKey nsFontDescriptor attribute =
  sendMessage nsFontDescriptor objectForKeySelector (toNSString attribute)

-- | @+ fontDescriptorWithFontAttributes:@
fontDescriptorWithFontAttributes :: IsNSDictionary attributes => attributes -> IO (Id NSFontDescriptor)
fontDescriptorWithFontAttributes attributes =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    sendClassMessage cls' fontDescriptorWithFontAttributesSelector (toNSDictionary attributes)

-- | @+ fontDescriptorWithName:size:@
fontDescriptorWithName_size :: IsNSString fontName => fontName -> CDouble -> IO (Id NSFontDescriptor)
fontDescriptorWithName_size fontName size =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    sendClassMessage cls' fontDescriptorWithName_sizeSelector (toNSString fontName) size

-- | @+ fontDescriptorWithName:matrix:@
fontDescriptorWithName_matrix :: (IsNSString fontName, IsNSAffineTransform matrix) => fontName -> matrix -> IO (Id NSFontDescriptor)
fontDescriptorWithName_matrix fontName matrix =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    sendClassMessage cls' fontDescriptorWithName_matrixSelector (toNSString fontName) (toNSAffineTransform matrix)

-- | @- initWithFontAttributes:@
initWithFontAttributes :: (IsNSFontDescriptor nsFontDescriptor, IsNSDictionary attributes) => nsFontDescriptor -> attributes -> IO (Id NSFontDescriptor)
initWithFontAttributes nsFontDescriptor attributes =
  sendOwnedMessage nsFontDescriptor initWithFontAttributesSelector (toNSDictionary attributes)

-- | @- matchingFontDescriptorsWithMandatoryKeys:@
matchingFontDescriptorsWithMandatoryKeys :: (IsNSFontDescriptor nsFontDescriptor, IsNSSet mandatoryKeys) => nsFontDescriptor -> mandatoryKeys -> IO (Id NSArray)
matchingFontDescriptorsWithMandatoryKeys nsFontDescriptor mandatoryKeys =
  sendMessage nsFontDescriptor matchingFontDescriptorsWithMandatoryKeysSelector (toNSSet mandatoryKeys)

-- | @- matchingFontDescriptorWithMandatoryKeys:@
matchingFontDescriptorWithMandatoryKeys :: (IsNSFontDescriptor nsFontDescriptor, IsNSSet mandatoryKeys) => nsFontDescriptor -> mandatoryKeys -> IO (Id NSFontDescriptor)
matchingFontDescriptorWithMandatoryKeys nsFontDescriptor mandatoryKeys =
  sendMessage nsFontDescriptor matchingFontDescriptorWithMandatoryKeysSelector (toNSSet mandatoryKeys)

-- | @- fontDescriptorByAddingAttributes:@
fontDescriptorByAddingAttributes :: (IsNSFontDescriptor nsFontDescriptor, IsNSDictionary attributes) => nsFontDescriptor -> attributes -> IO (Id NSFontDescriptor)
fontDescriptorByAddingAttributes nsFontDescriptor attributes =
  sendMessage nsFontDescriptor fontDescriptorByAddingAttributesSelector (toNSDictionary attributes)

-- | @- fontDescriptorWithSymbolicTraits:@
fontDescriptorWithSymbolicTraits :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> NSFontDescriptorSymbolicTraits -> IO (Id NSFontDescriptor)
fontDescriptorWithSymbolicTraits nsFontDescriptor symbolicTraits =
  sendMessage nsFontDescriptor fontDescriptorWithSymbolicTraitsSelector symbolicTraits

-- | @- fontDescriptorWithSize:@
fontDescriptorWithSize :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> CDouble -> IO (Id NSFontDescriptor)
fontDescriptorWithSize nsFontDescriptor newPointSize =
  sendMessage nsFontDescriptor fontDescriptorWithSizeSelector newPointSize

-- | @- fontDescriptorWithMatrix:@
fontDescriptorWithMatrix :: (IsNSFontDescriptor nsFontDescriptor, IsNSAffineTransform matrix) => nsFontDescriptor -> matrix -> IO (Id NSFontDescriptor)
fontDescriptorWithMatrix nsFontDescriptor matrix =
  sendMessage nsFontDescriptor fontDescriptorWithMatrixSelector (toNSAffineTransform matrix)

-- | @- fontDescriptorWithFace:@
fontDescriptorWithFace :: (IsNSFontDescriptor nsFontDescriptor, IsNSString newFace) => nsFontDescriptor -> newFace -> IO (Id NSFontDescriptor)
fontDescriptorWithFace nsFontDescriptor newFace =
  sendMessage nsFontDescriptor fontDescriptorWithFaceSelector (toNSString newFace)

-- | @- fontDescriptorWithFamily:@
fontDescriptorWithFamily :: (IsNSFontDescriptor nsFontDescriptor, IsNSString newFamily) => nsFontDescriptor -> newFamily -> IO (Id NSFontDescriptor)
fontDescriptorWithFamily nsFontDescriptor newFamily =
  sendMessage nsFontDescriptor fontDescriptorWithFamilySelector (toNSString newFamily)

-- | @- fontDescriptorWithDesign:@
fontDescriptorWithDesign :: (IsNSFontDescriptor nsFontDescriptor, IsNSString design) => nsFontDescriptor -> design -> IO (Id NSFontDescriptor)
fontDescriptorWithDesign nsFontDescriptor design =
  sendMessage nsFontDescriptor fontDescriptorWithDesignSelector (toNSString design)

-- | @+ preferredFontDescriptorForTextStyle:options:@
preferredFontDescriptorForTextStyle_options :: (IsNSString style, IsNSDictionary options) => style -> options -> IO (Id NSFontDescriptor)
preferredFontDescriptorForTextStyle_options style options =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    sendClassMessage cls' preferredFontDescriptorForTextStyle_optionsSelector (toNSString style) (toNSDictionary options)

-- | @- postscriptName@
postscriptName :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO (Id NSString)
postscriptName nsFontDescriptor =
  sendMessage nsFontDescriptor postscriptNameSelector

-- | @- pointSize@
pointSize :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO CDouble
pointSize nsFontDescriptor =
  sendMessage nsFontDescriptor pointSizeSelector

-- | @- matrix@
matrix :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO (Id NSAffineTransform)
matrix nsFontDescriptor =
  sendMessage nsFontDescriptor matrixSelector

-- | @- symbolicTraits@
symbolicTraits :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO NSFontDescriptorSymbolicTraits
symbolicTraits nsFontDescriptor =
  sendMessage nsFontDescriptor symbolicTraitsSelector

-- | @- requiresFontAssetRequest@
requiresFontAssetRequest :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO Bool
requiresFontAssetRequest nsFontDescriptor =
  sendMessage nsFontDescriptor requiresFontAssetRequestSelector

-- | @- fontAttributes@
fontAttributes :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO (Id NSDictionary)
fontAttributes nsFontDescriptor =
  sendMessage nsFontDescriptor fontAttributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector '[Id NSString] RawId
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @fontDescriptorWithFontAttributes:@
fontDescriptorWithFontAttributesSelector :: Selector '[Id NSDictionary] (Id NSFontDescriptor)
fontDescriptorWithFontAttributesSelector = mkSelector "fontDescriptorWithFontAttributes:"

-- | @Selector@ for @fontDescriptorWithName:size:@
fontDescriptorWithName_sizeSelector :: Selector '[Id NSString, CDouble] (Id NSFontDescriptor)
fontDescriptorWithName_sizeSelector = mkSelector "fontDescriptorWithName:size:"

-- | @Selector@ for @fontDescriptorWithName:matrix:@
fontDescriptorWithName_matrixSelector :: Selector '[Id NSString, Id NSAffineTransform] (Id NSFontDescriptor)
fontDescriptorWithName_matrixSelector = mkSelector "fontDescriptorWithName:matrix:"

-- | @Selector@ for @initWithFontAttributes:@
initWithFontAttributesSelector :: Selector '[Id NSDictionary] (Id NSFontDescriptor)
initWithFontAttributesSelector = mkSelector "initWithFontAttributes:"

-- | @Selector@ for @matchingFontDescriptorsWithMandatoryKeys:@
matchingFontDescriptorsWithMandatoryKeysSelector :: Selector '[Id NSSet] (Id NSArray)
matchingFontDescriptorsWithMandatoryKeysSelector = mkSelector "matchingFontDescriptorsWithMandatoryKeys:"

-- | @Selector@ for @matchingFontDescriptorWithMandatoryKeys:@
matchingFontDescriptorWithMandatoryKeysSelector :: Selector '[Id NSSet] (Id NSFontDescriptor)
matchingFontDescriptorWithMandatoryKeysSelector = mkSelector "matchingFontDescriptorWithMandatoryKeys:"

-- | @Selector@ for @fontDescriptorByAddingAttributes:@
fontDescriptorByAddingAttributesSelector :: Selector '[Id NSDictionary] (Id NSFontDescriptor)
fontDescriptorByAddingAttributesSelector = mkSelector "fontDescriptorByAddingAttributes:"

-- | @Selector@ for @fontDescriptorWithSymbolicTraits:@
fontDescriptorWithSymbolicTraitsSelector :: Selector '[NSFontDescriptorSymbolicTraits] (Id NSFontDescriptor)
fontDescriptorWithSymbolicTraitsSelector = mkSelector "fontDescriptorWithSymbolicTraits:"

-- | @Selector@ for @fontDescriptorWithSize:@
fontDescriptorWithSizeSelector :: Selector '[CDouble] (Id NSFontDescriptor)
fontDescriptorWithSizeSelector = mkSelector "fontDescriptorWithSize:"

-- | @Selector@ for @fontDescriptorWithMatrix:@
fontDescriptorWithMatrixSelector :: Selector '[Id NSAffineTransform] (Id NSFontDescriptor)
fontDescriptorWithMatrixSelector = mkSelector "fontDescriptorWithMatrix:"

-- | @Selector@ for @fontDescriptorWithFace:@
fontDescriptorWithFaceSelector :: Selector '[Id NSString] (Id NSFontDescriptor)
fontDescriptorWithFaceSelector = mkSelector "fontDescriptorWithFace:"

-- | @Selector@ for @fontDescriptorWithFamily:@
fontDescriptorWithFamilySelector :: Selector '[Id NSString] (Id NSFontDescriptor)
fontDescriptorWithFamilySelector = mkSelector "fontDescriptorWithFamily:"

-- | @Selector@ for @fontDescriptorWithDesign:@
fontDescriptorWithDesignSelector :: Selector '[Id NSString] (Id NSFontDescriptor)
fontDescriptorWithDesignSelector = mkSelector "fontDescriptorWithDesign:"

-- | @Selector@ for @preferredFontDescriptorForTextStyle:options:@
preferredFontDescriptorForTextStyle_optionsSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSFontDescriptor)
preferredFontDescriptorForTextStyle_optionsSelector = mkSelector "preferredFontDescriptorForTextStyle:options:"

-- | @Selector@ for @postscriptName@
postscriptNameSelector :: Selector '[] (Id NSString)
postscriptNameSelector = mkSelector "postscriptName"

-- | @Selector@ for @pointSize@
pointSizeSelector :: Selector '[] CDouble
pointSizeSelector = mkSelector "pointSize"

-- | @Selector@ for @matrix@
matrixSelector :: Selector '[] (Id NSAffineTransform)
matrixSelector = mkSelector "matrix"

-- | @Selector@ for @symbolicTraits@
symbolicTraitsSelector :: Selector '[] NSFontDescriptorSymbolicTraits
symbolicTraitsSelector = mkSelector "symbolicTraits"

-- | @Selector@ for @requiresFontAssetRequest@
requiresFontAssetRequestSelector :: Selector '[] Bool
requiresFontAssetRequestSelector = mkSelector "requiresFontAssetRequest"

-- | @Selector@ for @fontAttributes@
fontAttributesSelector :: Selector '[] (Id NSDictionary)
fontAttributesSelector = mkSelector "fontAttributes"

