{-# LANGUAGE PatternSynonyms #-}
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
  , objectForKeySelector
  , fontDescriptorWithFontAttributesSelector
  , fontDescriptorWithName_sizeSelector
  , fontDescriptorWithName_matrixSelector
  , initWithFontAttributesSelector
  , matchingFontDescriptorsWithMandatoryKeysSelector
  , matchingFontDescriptorWithMandatoryKeysSelector
  , fontDescriptorByAddingAttributesSelector
  , fontDescriptorWithSymbolicTraitsSelector
  , fontDescriptorWithSizeSelector
  , fontDescriptorWithMatrixSelector
  , fontDescriptorWithFaceSelector
  , fontDescriptorWithFamilySelector
  , fontDescriptorWithDesignSelector
  , preferredFontDescriptorForTextStyle_optionsSelector
  , postscriptNameSelector
  , pointSizeSelector
  , matrixSelector
  , symbolicTraitsSelector
  , requiresFontAssetRequestSelector
  , fontAttributesSelector

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

-- | @- objectForKey:@
objectForKey :: (IsNSFontDescriptor nsFontDescriptor, IsNSString attribute) => nsFontDescriptor -> attribute -> IO RawId
objectForKey nsFontDescriptor  attribute =
withObjCPtr attribute $ \raw_attribute ->
    fmap (RawId . castPtr) $ sendMsg nsFontDescriptor (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr raw_attribute :: Ptr ())]

-- | @+ fontDescriptorWithFontAttributes:@
fontDescriptorWithFontAttributes :: IsNSDictionary attributes => attributes -> IO (Id NSFontDescriptor)
fontDescriptorWithFontAttributes attributes =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    withObjCPtr attributes $ \raw_attributes ->
      sendClassMsg cls' (mkSelector "fontDescriptorWithFontAttributes:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fontDescriptorWithName:size:@
fontDescriptorWithName_size :: IsNSString fontName => fontName -> CDouble -> IO (Id NSFontDescriptor)
fontDescriptorWithName_size fontName size =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    withObjCPtr fontName $ \raw_fontName ->
      sendClassMsg cls' (mkSelector "fontDescriptorWithName:size:") (retPtr retVoid) [argPtr (castPtr raw_fontName :: Ptr ()), argCDouble (fromIntegral size)] >>= retainedObject . castPtr

-- | @+ fontDescriptorWithName:matrix:@
fontDescriptorWithName_matrix :: (IsNSString fontName, IsNSAffineTransform matrix) => fontName -> matrix -> IO (Id NSFontDescriptor)
fontDescriptorWithName_matrix fontName matrix =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    withObjCPtr fontName $ \raw_fontName ->
      withObjCPtr matrix $ \raw_matrix ->
        sendClassMsg cls' (mkSelector "fontDescriptorWithName:matrix:") (retPtr retVoid) [argPtr (castPtr raw_fontName :: Ptr ()), argPtr (castPtr raw_matrix :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithFontAttributes:@
initWithFontAttributes :: (IsNSFontDescriptor nsFontDescriptor, IsNSDictionary attributes) => nsFontDescriptor -> attributes -> IO (Id NSFontDescriptor)
initWithFontAttributes nsFontDescriptor  attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg nsFontDescriptor (mkSelector "initWithFontAttributes:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= ownedObject . castPtr

-- | @- matchingFontDescriptorsWithMandatoryKeys:@
matchingFontDescriptorsWithMandatoryKeys :: (IsNSFontDescriptor nsFontDescriptor, IsNSSet mandatoryKeys) => nsFontDescriptor -> mandatoryKeys -> IO (Id NSArray)
matchingFontDescriptorsWithMandatoryKeys nsFontDescriptor  mandatoryKeys =
withObjCPtr mandatoryKeys $ \raw_mandatoryKeys ->
    sendMsg nsFontDescriptor (mkSelector "matchingFontDescriptorsWithMandatoryKeys:") (retPtr retVoid) [argPtr (castPtr raw_mandatoryKeys :: Ptr ())] >>= retainedObject . castPtr

-- | @- matchingFontDescriptorWithMandatoryKeys:@
matchingFontDescriptorWithMandatoryKeys :: (IsNSFontDescriptor nsFontDescriptor, IsNSSet mandatoryKeys) => nsFontDescriptor -> mandatoryKeys -> IO (Id NSFontDescriptor)
matchingFontDescriptorWithMandatoryKeys nsFontDescriptor  mandatoryKeys =
withObjCPtr mandatoryKeys $ \raw_mandatoryKeys ->
    sendMsg nsFontDescriptor (mkSelector "matchingFontDescriptorWithMandatoryKeys:") (retPtr retVoid) [argPtr (castPtr raw_mandatoryKeys :: Ptr ())] >>= retainedObject . castPtr

-- | @- fontDescriptorByAddingAttributes:@
fontDescriptorByAddingAttributes :: (IsNSFontDescriptor nsFontDescriptor, IsNSDictionary attributes) => nsFontDescriptor -> attributes -> IO (Id NSFontDescriptor)
fontDescriptorByAddingAttributes nsFontDescriptor  attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg nsFontDescriptor (mkSelector "fontDescriptorByAddingAttributes:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @- fontDescriptorWithSymbolicTraits:@
fontDescriptorWithSymbolicTraits :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> NSFontDescriptorSymbolicTraits -> IO (Id NSFontDescriptor)
fontDescriptorWithSymbolicTraits nsFontDescriptor  symbolicTraits =
  sendMsg nsFontDescriptor (mkSelector "fontDescriptorWithSymbolicTraits:") (retPtr retVoid) [argCUInt (coerce symbolicTraits)] >>= retainedObject . castPtr

-- | @- fontDescriptorWithSize:@
fontDescriptorWithSize :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> CDouble -> IO (Id NSFontDescriptor)
fontDescriptorWithSize nsFontDescriptor  newPointSize =
  sendMsg nsFontDescriptor (mkSelector "fontDescriptorWithSize:") (retPtr retVoid) [argCDouble (fromIntegral newPointSize)] >>= retainedObject . castPtr

-- | @- fontDescriptorWithMatrix:@
fontDescriptorWithMatrix :: (IsNSFontDescriptor nsFontDescriptor, IsNSAffineTransform matrix) => nsFontDescriptor -> matrix -> IO (Id NSFontDescriptor)
fontDescriptorWithMatrix nsFontDescriptor  matrix =
withObjCPtr matrix $ \raw_matrix ->
    sendMsg nsFontDescriptor (mkSelector "fontDescriptorWithMatrix:") (retPtr retVoid) [argPtr (castPtr raw_matrix :: Ptr ())] >>= retainedObject . castPtr

-- | @- fontDescriptorWithFace:@
fontDescriptorWithFace :: (IsNSFontDescriptor nsFontDescriptor, IsNSString newFace) => nsFontDescriptor -> newFace -> IO (Id NSFontDescriptor)
fontDescriptorWithFace nsFontDescriptor  newFace =
withObjCPtr newFace $ \raw_newFace ->
    sendMsg nsFontDescriptor (mkSelector "fontDescriptorWithFace:") (retPtr retVoid) [argPtr (castPtr raw_newFace :: Ptr ())] >>= retainedObject . castPtr

-- | @- fontDescriptorWithFamily:@
fontDescriptorWithFamily :: (IsNSFontDescriptor nsFontDescriptor, IsNSString newFamily) => nsFontDescriptor -> newFamily -> IO (Id NSFontDescriptor)
fontDescriptorWithFamily nsFontDescriptor  newFamily =
withObjCPtr newFamily $ \raw_newFamily ->
    sendMsg nsFontDescriptor (mkSelector "fontDescriptorWithFamily:") (retPtr retVoid) [argPtr (castPtr raw_newFamily :: Ptr ())] >>= retainedObject . castPtr

-- | @- fontDescriptorWithDesign:@
fontDescriptorWithDesign :: (IsNSFontDescriptor nsFontDescriptor, IsNSString design) => nsFontDescriptor -> design -> IO (Id NSFontDescriptor)
fontDescriptorWithDesign nsFontDescriptor  design =
withObjCPtr design $ \raw_design ->
    sendMsg nsFontDescriptor (mkSelector "fontDescriptorWithDesign:") (retPtr retVoid) [argPtr (castPtr raw_design :: Ptr ())] >>= retainedObject . castPtr

-- | @+ preferredFontDescriptorForTextStyle:options:@
preferredFontDescriptorForTextStyle_options :: (IsNSString style, IsNSDictionary options) => style -> options -> IO (Id NSFontDescriptor)
preferredFontDescriptorForTextStyle_options style options =
  do
    cls' <- getRequiredClass "NSFontDescriptor"
    withObjCPtr style $ \raw_style ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "preferredFontDescriptorForTextStyle:options:") (retPtr retVoid) [argPtr (castPtr raw_style :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- postscriptName@
postscriptName :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO (Id NSString)
postscriptName nsFontDescriptor  =
  sendMsg nsFontDescriptor (mkSelector "postscriptName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pointSize@
pointSize :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO CDouble
pointSize nsFontDescriptor  =
  sendMsg nsFontDescriptor (mkSelector "pointSize") retCDouble []

-- | @- matrix@
matrix :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO (Id NSAffineTransform)
matrix nsFontDescriptor  =
  sendMsg nsFontDescriptor (mkSelector "matrix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- symbolicTraits@
symbolicTraits :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO NSFontDescriptorSymbolicTraits
symbolicTraits nsFontDescriptor  =
  fmap (coerce :: CUInt -> NSFontDescriptorSymbolicTraits) $ sendMsg nsFontDescriptor (mkSelector "symbolicTraits") retCUInt []

-- | @- requiresFontAssetRequest@
requiresFontAssetRequest :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO Bool
requiresFontAssetRequest nsFontDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFontDescriptor (mkSelector "requiresFontAssetRequest") retCULong []

-- | @- fontAttributes@
fontAttributes :: IsNSFontDescriptor nsFontDescriptor => nsFontDescriptor -> IO (Id NSDictionary)
fontAttributes nsFontDescriptor  =
  sendMsg nsFontDescriptor (mkSelector "fontAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @fontDescriptorWithFontAttributes:@
fontDescriptorWithFontAttributesSelector :: Selector
fontDescriptorWithFontAttributesSelector = mkSelector "fontDescriptorWithFontAttributes:"

-- | @Selector@ for @fontDescriptorWithName:size:@
fontDescriptorWithName_sizeSelector :: Selector
fontDescriptorWithName_sizeSelector = mkSelector "fontDescriptorWithName:size:"

-- | @Selector@ for @fontDescriptorWithName:matrix:@
fontDescriptorWithName_matrixSelector :: Selector
fontDescriptorWithName_matrixSelector = mkSelector "fontDescriptorWithName:matrix:"

-- | @Selector@ for @initWithFontAttributes:@
initWithFontAttributesSelector :: Selector
initWithFontAttributesSelector = mkSelector "initWithFontAttributes:"

-- | @Selector@ for @matchingFontDescriptorsWithMandatoryKeys:@
matchingFontDescriptorsWithMandatoryKeysSelector :: Selector
matchingFontDescriptorsWithMandatoryKeysSelector = mkSelector "matchingFontDescriptorsWithMandatoryKeys:"

-- | @Selector@ for @matchingFontDescriptorWithMandatoryKeys:@
matchingFontDescriptorWithMandatoryKeysSelector :: Selector
matchingFontDescriptorWithMandatoryKeysSelector = mkSelector "matchingFontDescriptorWithMandatoryKeys:"

-- | @Selector@ for @fontDescriptorByAddingAttributes:@
fontDescriptorByAddingAttributesSelector :: Selector
fontDescriptorByAddingAttributesSelector = mkSelector "fontDescriptorByAddingAttributes:"

-- | @Selector@ for @fontDescriptorWithSymbolicTraits:@
fontDescriptorWithSymbolicTraitsSelector :: Selector
fontDescriptorWithSymbolicTraitsSelector = mkSelector "fontDescriptorWithSymbolicTraits:"

-- | @Selector@ for @fontDescriptorWithSize:@
fontDescriptorWithSizeSelector :: Selector
fontDescriptorWithSizeSelector = mkSelector "fontDescriptorWithSize:"

-- | @Selector@ for @fontDescriptorWithMatrix:@
fontDescriptorWithMatrixSelector :: Selector
fontDescriptorWithMatrixSelector = mkSelector "fontDescriptorWithMatrix:"

-- | @Selector@ for @fontDescriptorWithFace:@
fontDescriptorWithFaceSelector :: Selector
fontDescriptorWithFaceSelector = mkSelector "fontDescriptorWithFace:"

-- | @Selector@ for @fontDescriptorWithFamily:@
fontDescriptorWithFamilySelector :: Selector
fontDescriptorWithFamilySelector = mkSelector "fontDescriptorWithFamily:"

-- | @Selector@ for @fontDescriptorWithDesign:@
fontDescriptorWithDesignSelector :: Selector
fontDescriptorWithDesignSelector = mkSelector "fontDescriptorWithDesign:"

-- | @Selector@ for @preferredFontDescriptorForTextStyle:options:@
preferredFontDescriptorForTextStyle_optionsSelector :: Selector
preferredFontDescriptorForTextStyle_optionsSelector = mkSelector "preferredFontDescriptorForTextStyle:options:"

-- | @Selector@ for @postscriptName@
postscriptNameSelector :: Selector
postscriptNameSelector = mkSelector "postscriptName"

-- | @Selector@ for @pointSize@
pointSizeSelector :: Selector
pointSizeSelector = mkSelector "pointSize"

-- | @Selector@ for @matrix@
matrixSelector :: Selector
matrixSelector = mkSelector "matrix"

-- | @Selector@ for @symbolicTraits@
symbolicTraitsSelector :: Selector
symbolicTraitsSelector = mkSelector "symbolicTraits"

-- | @Selector@ for @requiresFontAssetRequest@
requiresFontAssetRequestSelector :: Selector
requiresFontAssetRequestSelector = mkSelector "requiresFontAssetRequest"

-- | @Selector@ for @fontAttributes@
fontAttributesSelector :: Selector
fontAttributesSelector = mkSelector "fontAttributes"

