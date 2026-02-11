{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableCharacterSet@.
module ObjC.Foundation.NSMutableCharacterSet
  ( NSMutableCharacterSet
  , IsNSMutableCharacterSet(..)
  , addCharactersInRange
  , removeCharactersInRange
  , addCharactersInString
  , removeCharactersInString
  , formUnionWithCharacterSet
  , formIntersectionWithCharacterSet
  , invert
  , controlCharacterSet
  , whitespaceCharacterSet
  , whitespaceAndNewlineCharacterSet
  , decimalDigitCharacterSet
  , letterCharacterSet
  , lowercaseLetterCharacterSet
  , uppercaseLetterCharacterSet
  , nonBaseCharacterSet
  , alphanumericCharacterSet
  , decomposableCharacterSet
  , illegalCharacterSet
  , punctuationCharacterSet
  , capitalizedLetterCharacterSet
  , symbolCharacterSet
  , newlineCharacterSet
  , characterSetWithRange
  , characterSetWithCharactersInString
  , characterSetWithBitmapRepresentation
  , characterSetWithContentsOfFile
  , addCharactersInRangeSelector
  , removeCharactersInRangeSelector
  , addCharactersInStringSelector
  , removeCharactersInStringSelector
  , formUnionWithCharacterSetSelector
  , formIntersectionWithCharacterSetSelector
  , invertSelector
  , controlCharacterSetSelector
  , whitespaceCharacterSetSelector
  , whitespaceAndNewlineCharacterSetSelector
  , decimalDigitCharacterSetSelector
  , letterCharacterSetSelector
  , lowercaseLetterCharacterSetSelector
  , uppercaseLetterCharacterSetSelector
  , nonBaseCharacterSetSelector
  , alphanumericCharacterSetSelector
  , decomposableCharacterSetSelector
  , illegalCharacterSetSelector
  , punctuationCharacterSetSelector
  , capitalizedLetterCharacterSetSelector
  , symbolCharacterSetSelector
  , newlineCharacterSetSelector
  , characterSetWithRangeSelector
  , characterSetWithCharactersInStringSelector
  , characterSetWithBitmapRepresentationSelector
  , characterSetWithContentsOfFileSelector


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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- addCharactersInRange:@
addCharactersInRange :: IsNSMutableCharacterSet nsMutableCharacterSet => nsMutableCharacterSet -> NSRange -> IO ()
addCharactersInRange nsMutableCharacterSet  aRange =
  sendMsg nsMutableCharacterSet (mkSelector "addCharactersInRange:") retVoid [argNSRange aRange]

-- | @- removeCharactersInRange:@
removeCharactersInRange :: IsNSMutableCharacterSet nsMutableCharacterSet => nsMutableCharacterSet -> NSRange -> IO ()
removeCharactersInRange nsMutableCharacterSet  aRange =
  sendMsg nsMutableCharacterSet (mkSelector "removeCharactersInRange:") retVoid [argNSRange aRange]

-- | @- addCharactersInString:@
addCharactersInString :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSString aString) => nsMutableCharacterSet -> aString -> IO ()
addCharactersInString nsMutableCharacterSet  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsMutableCharacterSet (mkSelector "addCharactersInString:") retVoid [argPtr (castPtr raw_aString :: Ptr ())]

-- | @- removeCharactersInString:@
removeCharactersInString :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSString aString) => nsMutableCharacterSet -> aString -> IO ()
removeCharactersInString nsMutableCharacterSet  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsMutableCharacterSet (mkSelector "removeCharactersInString:") retVoid [argPtr (castPtr raw_aString :: Ptr ())]

-- | @- formUnionWithCharacterSet:@
formUnionWithCharacterSet :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSCharacterSet otherSet) => nsMutableCharacterSet -> otherSet -> IO ()
formUnionWithCharacterSet nsMutableCharacterSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    sendMsg nsMutableCharacterSet (mkSelector "formUnionWithCharacterSet:") retVoid [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- formIntersectionWithCharacterSet:@
formIntersectionWithCharacterSet :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSCharacterSet otherSet) => nsMutableCharacterSet -> otherSet -> IO ()
formIntersectionWithCharacterSet nsMutableCharacterSet  otherSet =
withObjCPtr otherSet $ \raw_otherSet ->
    sendMsg nsMutableCharacterSet (mkSelector "formIntersectionWithCharacterSet:") retVoid [argPtr (castPtr raw_otherSet :: Ptr ())]

-- | @- invert@
invert :: IsNSMutableCharacterSet nsMutableCharacterSet => nsMutableCharacterSet -> IO ()
invert nsMutableCharacterSet  =
  sendMsg nsMutableCharacterSet (mkSelector "invert") retVoid []

-- | @+ controlCharacterSet@
controlCharacterSet :: IO (Id NSMutableCharacterSet)
controlCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "controlCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ whitespaceCharacterSet@
whitespaceCharacterSet :: IO (Id NSMutableCharacterSet)
whitespaceCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "whitespaceCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSet :: IO (Id NSMutableCharacterSet)
whitespaceAndNewlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "whitespaceAndNewlineCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decimalDigitCharacterSet@
decimalDigitCharacterSet :: IO (Id NSMutableCharacterSet)
decimalDigitCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "decimalDigitCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ letterCharacterSet@
letterCharacterSet :: IO (Id NSMutableCharacterSet)
letterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "letterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ lowercaseLetterCharacterSet@
lowercaseLetterCharacterSet :: IO (Id NSMutableCharacterSet)
lowercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "lowercaseLetterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ uppercaseLetterCharacterSet@
uppercaseLetterCharacterSet :: IO (Id NSMutableCharacterSet)
uppercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "uppercaseLetterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nonBaseCharacterSet@
nonBaseCharacterSet :: IO (Id NSMutableCharacterSet)
nonBaseCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "nonBaseCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ alphanumericCharacterSet@
alphanumericCharacterSet :: IO (Id NSMutableCharacterSet)
alphanumericCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "alphanumericCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decomposableCharacterSet@
decomposableCharacterSet :: IO (Id NSMutableCharacterSet)
decomposableCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "decomposableCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ illegalCharacterSet@
illegalCharacterSet :: IO (Id NSMutableCharacterSet)
illegalCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "illegalCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ punctuationCharacterSet@
punctuationCharacterSet :: IO (Id NSMutableCharacterSet)
punctuationCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "punctuationCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ capitalizedLetterCharacterSet@
capitalizedLetterCharacterSet :: IO (Id NSMutableCharacterSet)
capitalizedLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "capitalizedLetterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ symbolCharacterSet@
symbolCharacterSet :: IO (Id NSMutableCharacterSet)
symbolCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "symbolCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ newlineCharacterSet@
newlineCharacterSet :: IO (Id NSMutableCharacterSet)
newlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "newlineCharacterSet") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ characterSetWithRange:@
characterSetWithRange :: NSRange -> IO (Id NSMutableCharacterSet)
characterSetWithRange aRange =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMsg cls' (mkSelector "characterSetWithRange:") (retPtr retVoid) [argNSRange aRange] >>= retainedObject . castPtr

-- | @+ characterSetWithCharactersInString:@
characterSetWithCharactersInString :: IsNSString aString => aString -> IO (Id NSMutableCharacterSet)
characterSetWithCharactersInString aString =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    withObjCPtr aString $ \raw_aString ->
      sendClassMsg cls' (mkSelector "characterSetWithCharactersInString:") (retPtr retVoid) [argPtr (castPtr raw_aString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentation :: IsNSData data_ => data_ -> IO (Id NSMutableCharacterSet)
characterSetWithBitmapRepresentation data_ =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "characterSetWithBitmapRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ characterSetWithContentsOfFile:@
characterSetWithContentsOfFile :: IsNSString fName => fName -> IO (Id NSMutableCharacterSet)
characterSetWithContentsOfFile fName =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    withObjCPtr fName $ \raw_fName ->
      sendClassMsg cls' (mkSelector "characterSetWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_fName :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addCharactersInRange:@
addCharactersInRangeSelector :: Selector
addCharactersInRangeSelector = mkSelector "addCharactersInRange:"

-- | @Selector@ for @removeCharactersInRange:@
removeCharactersInRangeSelector :: Selector
removeCharactersInRangeSelector = mkSelector "removeCharactersInRange:"

-- | @Selector@ for @addCharactersInString:@
addCharactersInStringSelector :: Selector
addCharactersInStringSelector = mkSelector "addCharactersInString:"

-- | @Selector@ for @removeCharactersInString:@
removeCharactersInStringSelector :: Selector
removeCharactersInStringSelector = mkSelector "removeCharactersInString:"

-- | @Selector@ for @formUnionWithCharacterSet:@
formUnionWithCharacterSetSelector :: Selector
formUnionWithCharacterSetSelector = mkSelector "formUnionWithCharacterSet:"

-- | @Selector@ for @formIntersectionWithCharacterSet:@
formIntersectionWithCharacterSetSelector :: Selector
formIntersectionWithCharacterSetSelector = mkSelector "formIntersectionWithCharacterSet:"

-- | @Selector@ for @invert@
invertSelector :: Selector
invertSelector = mkSelector "invert"

-- | @Selector@ for @controlCharacterSet@
controlCharacterSetSelector :: Selector
controlCharacterSetSelector = mkSelector "controlCharacterSet"

-- | @Selector@ for @whitespaceCharacterSet@
whitespaceCharacterSetSelector :: Selector
whitespaceCharacterSetSelector = mkSelector "whitespaceCharacterSet"

-- | @Selector@ for @whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSetSelector :: Selector
whitespaceAndNewlineCharacterSetSelector = mkSelector "whitespaceAndNewlineCharacterSet"

-- | @Selector@ for @decimalDigitCharacterSet@
decimalDigitCharacterSetSelector :: Selector
decimalDigitCharacterSetSelector = mkSelector "decimalDigitCharacterSet"

-- | @Selector@ for @letterCharacterSet@
letterCharacterSetSelector :: Selector
letterCharacterSetSelector = mkSelector "letterCharacterSet"

-- | @Selector@ for @lowercaseLetterCharacterSet@
lowercaseLetterCharacterSetSelector :: Selector
lowercaseLetterCharacterSetSelector = mkSelector "lowercaseLetterCharacterSet"

-- | @Selector@ for @uppercaseLetterCharacterSet@
uppercaseLetterCharacterSetSelector :: Selector
uppercaseLetterCharacterSetSelector = mkSelector "uppercaseLetterCharacterSet"

-- | @Selector@ for @nonBaseCharacterSet@
nonBaseCharacterSetSelector :: Selector
nonBaseCharacterSetSelector = mkSelector "nonBaseCharacterSet"

-- | @Selector@ for @alphanumericCharacterSet@
alphanumericCharacterSetSelector :: Selector
alphanumericCharacterSetSelector = mkSelector "alphanumericCharacterSet"

-- | @Selector@ for @decomposableCharacterSet@
decomposableCharacterSetSelector :: Selector
decomposableCharacterSetSelector = mkSelector "decomposableCharacterSet"

-- | @Selector@ for @illegalCharacterSet@
illegalCharacterSetSelector :: Selector
illegalCharacterSetSelector = mkSelector "illegalCharacterSet"

-- | @Selector@ for @punctuationCharacterSet@
punctuationCharacterSetSelector :: Selector
punctuationCharacterSetSelector = mkSelector "punctuationCharacterSet"

-- | @Selector@ for @capitalizedLetterCharacterSet@
capitalizedLetterCharacterSetSelector :: Selector
capitalizedLetterCharacterSetSelector = mkSelector "capitalizedLetterCharacterSet"

-- | @Selector@ for @symbolCharacterSet@
symbolCharacterSetSelector :: Selector
symbolCharacterSetSelector = mkSelector "symbolCharacterSet"

-- | @Selector@ for @newlineCharacterSet@
newlineCharacterSetSelector :: Selector
newlineCharacterSetSelector = mkSelector "newlineCharacterSet"

-- | @Selector@ for @characterSetWithRange:@
characterSetWithRangeSelector :: Selector
characterSetWithRangeSelector = mkSelector "characterSetWithRange:"

-- | @Selector@ for @characterSetWithCharactersInString:@
characterSetWithCharactersInStringSelector :: Selector
characterSetWithCharactersInStringSelector = mkSelector "characterSetWithCharactersInString:"

-- | @Selector@ for @characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentationSelector :: Selector
characterSetWithBitmapRepresentationSelector = mkSelector "characterSetWithBitmapRepresentation:"

-- | @Selector@ for @characterSetWithContentsOfFile:@
characterSetWithContentsOfFileSelector :: Selector
characterSetWithContentsOfFileSelector = mkSelector "characterSetWithContentsOfFile:"

