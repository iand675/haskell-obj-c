{-# LANGUAGE DataKinds #-}
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
  , addCharactersInStringSelector
  , alphanumericCharacterSetSelector
  , capitalizedLetterCharacterSetSelector
  , characterSetWithBitmapRepresentationSelector
  , characterSetWithCharactersInStringSelector
  , characterSetWithContentsOfFileSelector
  , characterSetWithRangeSelector
  , controlCharacterSetSelector
  , decimalDigitCharacterSetSelector
  , decomposableCharacterSetSelector
  , formIntersectionWithCharacterSetSelector
  , formUnionWithCharacterSetSelector
  , illegalCharacterSetSelector
  , invertSelector
  , letterCharacterSetSelector
  , lowercaseLetterCharacterSetSelector
  , newlineCharacterSetSelector
  , nonBaseCharacterSetSelector
  , punctuationCharacterSetSelector
  , removeCharactersInRangeSelector
  , removeCharactersInStringSelector
  , symbolCharacterSetSelector
  , uppercaseLetterCharacterSetSelector
  , whitespaceAndNewlineCharacterSetSelector
  , whitespaceCharacterSetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- addCharactersInRange:@
addCharactersInRange :: IsNSMutableCharacterSet nsMutableCharacterSet => nsMutableCharacterSet -> NSRange -> IO ()
addCharactersInRange nsMutableCharacterSet aRange =
  sendMessage nsMutableCharacterSet addCharactersInRangeSelector aRange

-- | @- removeCharactersInRange:@
removeCharactersInRange :: IsNSMutableCharacterSet nsMutableCharacterSet => nsMutableCharacterSet -> NSRange -> IO ()
removeCharactersInRange nsMutableCharacterSet aRange =
  sendMessage nsMutableCharacterSet removeCharactersInRangeSelector aRange

-- | @- addCharactersInString:@
addCharactersInString :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSString aString) => nsMutableCharacterSet -> aString -> IO ()
addCharactersInString nsMutableCharacterSet aString =
  sendMessage nsMutableCharacterSet addCharactersInStringSelector (toNSString aString)

-- | @- removeCharactersInString:@
removeCharactersInString :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSString aString) => nsMutableCharacterSet -> aString -> IO ()
removeCharactersInString nsMutableCharacterSet aString =
  sendMessage nsMutableCharacterSet removeCharactersInStringSelector (toNSString aString)

-- | @- formUnionWithCharacterSet:@
formUnionWithCharacterSet :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSCharacterSet otherSet) => nsMutableCharacterSet -> otherSet -> IO ()
formUnionWithCharacterSet nsMutableCharacterSet otherSet =
  sendMessage nsMutableCharacterSet formUnionWithCharacterSetSelector (toNSCharacterSet otherSet)

-- | @- formIntersectionWithCharacterSet:@
formIntersectionWithCharacterSet :: (IsNSMutableCharacterSet nsMutableCharacterSet, IsNSCharacterSet otherSet) => nsMutableCharacterSet -> otherSet -> IO ()
formIntersectionWithCharacterSet nsMutableCharacterSet otherSet =
  sendMessage nsMutableCharacterSet formIntersectionWithCharacterSetSelector (toNSCharacterSet otherSet)

-- | @- invert@
invert :: IsNSMutableCharacterSet nsMutableCharacterSet => nsMutableCharacterSet -> IO ()
invert nsMutableCharacterSet =
  sendMessage nsMutableCharacterSet invertSelector

-- | @+ controlCharacterSet@
controlCharacterSet :: IO (Id NSMutableCharacterSet)
controlCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' controlCharacterSetSelector

-- | @+ whitespaceCharacterSet@
whitespaceCharacterSet :: IO (Id NSMutableCharacterSet)
whitespaceCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' whitespaceCharacterSetSelector

-- | @+ whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSet :: IO (Id NSMutableCharacterSet)
whitespaceAndNewlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' whitespaceAndNewlineCharacterSetSelector

-- | @+ decimalDigitCharacterSet@
decimalDigitCharacterSet :: IO (Id NSMutableCharacterSet)
decimalDigitCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' decimalDigitCharacterSetSelector

-- | @+ letterCharacterSet@
letterCharacterSet :: IO (Id NSMutableCharacterSet)
letterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' letterCharacterSetSelector

-- | @+ lowercaseLetterCharacterSet@
lowercaseLetterCharacterSet :: IO (Id NSMutableCharacterSet)
lowercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' lowercaseLetterCharacterSetSelector

-- | @+ uppercaseLetterCharacterSet@
uppercaseLetterCharacterSet :: IO (Id NSMutableCharacterSet)
uppercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' uppercaseLetterCharacterSetSelector

-- | @+ nonBaseCharacterSet@
nonBaseCharacterSet :: IO (Id NSMutableCharacterSet)
nonBaseCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' nonBaseCharacterSetSelector

-- | @+ alphanumericCharacterSet@
alphanumericCharacterSet :: IO (Id NSMutableCharacterSet)
alphanumericCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' alphanumericCharacterSetSelector

-- | @+ decomposableCharacterSet@
decomposableCharacterSet :: IO (Id NSMutableCharacterSet)
decomposableCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' decomposableCharacterSetSelector

-- | @+ illegalCharacterSet@
illegalCharacterSet :: IO (Id NSMutableCharacterSet)
illegalCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' illegalCharacterSetSelector

-- | @+ punctuationCharacterSet@
punctuationCharacterSet :: IO (Id NSMutableCharacterSet)
punctuationCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' punctuationCharacterSetSelector

-- | @+ capitalizedLetterCharacterSet@
capitalizedLetterCharacterSet :: IO (Id NSMutableCharacterSet)
capitalizedLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' capitalizedLetterCharacterSetSelector

-- | @+ symbolCharacterSet@
symbolCharacterSet :: IO (Id NSMutableCharacterSet)
symbolCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' symbolCharacterSetSelector

-- | @+ newlineCharacterSet@
newlineCharacterSet :: IO (Id NSMutableCharacterSet)
newlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendOwnedClassMessage cls' newlineCharacterSetSelector

-- | @+ characterSetWithRange:@
characterSetWithRange :: NSRange -> IO (Id NSMutableCharacterSet)
characterSetWithRange aRange =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' characterSetWithRangeSelector aRange

-- | @+ characterSetWithCharactersInString:@
characterSetWithCharactersInString :: IsNSString aString => aString -> IO (Id NSMutableCharacterSet)
characterSetWithCharactersInString aString =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' characterSetWithCharactersInStringSelector (toNSString aString)

-- | @+ characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentation :: IsNSData data_ => data_ -> IO (Id NSMutableCharacterSet)
characterSetWithBitmapRepresentation data_ =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' characterSetWithBitmapRepresentationSelector (toNSData data_)

-- | @+ characterSetWithContentsOfFile:@
characterSetWithContentsOfFile :: IsNSString fName => fName -> IO (Id NSMutableCharacterSet)
characterSetWithContentsOfFile fName =
  do
    cls' <- getRequiredClass "NSMutableCharacterSet"
    sendClassMessage cls' characterSetWithContentsOfFileSelector (toNSString fName)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addCharactersInRange:@
addCharactersInRangeSelector :: Selector '[NSRange] ()
addCharactersInRangeSelector = mkSelector "addCharactersInRange:"

-- | @Selector@ for @removeCharactersInRange:@
removeCharactersInRangeSelector :: Selector '[NSRange] ()
removeCharactersInRangeSelector = mkSelector "removeCharactersInRange:"

-- | @Selector@ for @addCharactersInString:@
addCharactersInStringSelector :: Selector '[Id NSString] ()
addCharactersInStringSelector = mkSelector "addCharactersInString:"

-- | @Selector@ for @removeCharactersInString:@
removeCharactersInStringSelector :: Selector '[Id NSString] ()
removeCharactersInStringSelector = mkSelector "removeCharactersInString:"

-- | @Selector@ for @formUnionWithCharacterSet:@
formUnionWithCharacterSetSelector :: Selector '[Id NSCharacterSet] ()
formUnionWithCharacterSetSelector = mkSelector "formUnionWithCharacterSet:"

-- | @Selector@ for @formIntersectionWithCharacterSet:@
formIntersectionWithCharacterSetSelector :: Selector '[Id NSCharacterSet] ()
formIntersectionWithCharacterSetSelector = mkSelector "formIntersectionWithCharacterSet:"

-- | @Selector@ for @invert@
invertSelector :: Selector '[] ()
invertSelector = mkSelector "invert"

-- | @Selector@ for @controlCharacterSet@
controlCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
controlCharacterSetSelector = mkSelector "controlCharacterSet"

-- | @Selector@ for @whitespaceCharacterSet@
whitespaceCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
whitespaceCharacterSetSelector = mkSelector "whitespaceCharacterSet"

-- | @Selector@ for @whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
whitespaceAndNewlineCharacterSetSelector = mkSelector "whitespaceAndNewlineCharacterSet"

-- | @Selector@ for @decimalDigitCharacterSet@
decimalDigitCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
decimalDigitCharacterSetSelector = mkSelector "decimalDigitCharacterSet"

-- | @Selector@ for @letterCharacterSet@
letterCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
letterCharacterSetSelector = mkSelector "letterCharacterSet"

-- | @Selector@ for @lowercaseLetterCharacterSet@
lowercaseLetterCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
lowercaseLetterCharacterSetSelector = mkSelector "lowercaseLetterCharacterSet"

-- | @Selector@ for @uppercaseLetterCharacterSet@
uppercaseLetterCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
uppercaseLetterCharacterSetSelector = mkSelector "uppercaseLetterCharacterSet"

-- | @Selector@ for @nonBaseCharacterSet@
nonBaseCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
nonBaseCharacterSetSelector = mkSelector "nonBaseCharacterSet"

-- | @Selector@ for @alphanumericCharacterSet@
alphanumericCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
alphanumericCharacterSetSelector = mkSelector "alphanumericCharacterSet"

-- | @Selector@ for @decomposableCharacterSet@
decomposableCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
decomposableCharacterSetSelector = mkSelector "decomposableCharacterSet"

-- | @Selector@ for @illegalCharacterSet@
illegalCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
illegalCharacterSetSelector = mkSelector "illegalCharacterSet"

-- | @Selector@ for @punctuationCharacterSet@
punctuationCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
punctuationCharacterSetSelector = mkSelector "punctuationCharacterSet"

-- | @Selector@ for @capitalizedLetterCharacterSet@
capitalizedLetterCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
capitalizedLetterCharacterSetSelector = mkSelector "capitalizedLetterCharacterSet"

-- | @Selector@ for @symbolCharacterSet@
symbolCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
symbolCharacterSetSelector = mkSelector "symbolCharacterSet"

-- | @Selector@ for @newlineCharacterSet@
newlineCharacterSetSelector :: Selector '[] (Id NSMutableCharacterSet)
newlineCharacterSetSelector = mkSelector "newlineCharacterSet"

-- | @Selector@ for @characterSetWithRange:@
characterSetWithRangeSelector :: Selector '[NSRange] (Id NSMutableCharacterSet)
characterSetWithRangeSelector = mkSelector "characterSetWithRange:"

-- | @Selector@ for @characterSetWithCharactersInString:@
characterSetWithCharactersInStringSelector :: Selector '[Id NSString] (Id NSMutableCharacterSet)
characterSetWithCharactersInStringSelector = mkSelector "characterSetWithCharactersInString:"

-- | @Selector@ for @characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentationSelector :: Selector '[Id NSData] (Id NSMutableCharacterSet)
characterSetWithBitmapRepresentationSelector = mkSelector "characterSetWithBitmapRepresentation:"

-- | @Selector@ for @characterSetWithContentsOfFile:@
characterSetWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSMutableCharacterSet)
characterSetWithContentsOfFileSelector = mkSelector "characterSetWithContentsOfFile:"

