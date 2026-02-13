{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCharacterSet@.
module ObjC.Foundation.NSCharacterSet
  ( NSCharacterSet
  , IsNSCharacterSet(..)
  , characterSetWithRange
  , characterSetWithCharactersInString
  , characterSetWithBitmapRepresentation
  , characterSetWithContentsOfFile
  , initWithCoder
  , characterIsMember
  , longCharacterIsMember
  , isSupersetOfSet
  , hasMemberInPlane
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
  , bitmapRepresentation
  , invertedSet
  , urlUserAllowedCharacterSet
  , urlPasswordAllowedCharacterSet
  , urlHostAllowedCharacterSet
  , urlPathAllowedCharacterSet
  , urlQueryAllowedCharacterSet
  , urlFragmentAllowedCharacterSet
  , alphanumericCharacterSetSelector
  , bitmapRepresentationSelector
  , capitalizedLetterCharacterSetSelector
  , characterIsMemberSelector
  , characterSetWithBitmapRepresentationSelector
  , characterSetWithCharactersInStringSelector
  , characterSetWithContentsOfFileSelector
  , characterSetWithRangeSelector
  , controlCharacterSetSelector
  , decimalDigitCharacterSetSelector
  , decomposableCharacterSetSelector
  , hasMemberInPlaneSelector
  , illegalCharacterSetSelector
  , initWithCoderSelector
  , invertedSetSelector
  , isSupersetOfSetSelector
  , letterCharacterSetSelector
  , longCharacterIsMemberSelector
  , lowercaseLetterCharacterSetSelector
  , newlineCharacterSetSelector
  , nonBaseCharacterSetSelector
  , punctuationCharacterSetSelector
  , symbolCharacterSetSelector
  , uppercaseLetterCharacterSetSelector
  , urlFragmentAllowedCharacterSetSelector
  , urlHostAllowedCharacterSetSelector
  , urlPasswordAllowedCharacterSetSelector
  , urlPathAllowedCharacterSetSelector
  , urlQueryAllowedCharacterSetSelector
  , urlUserAllowedCharacterSetSelector
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

-- | @+ characterSetWithRange:@
characterSetWithRange :: NSRange -> IO (Id NSCharacterSet)
characterSetWithRange aRange =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' characterSetWithRangeSelector aRange

-- | @+ characterSetWithCharactersInString:@
characterSetWithCharactersInString :: IsNSString aString => aString -> IO (Id NSCharacterSet)
characterSetWithCharactersInString aString =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' characterSetWithCharactersInStringSelector (toNSString aString)

-- | @+ characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentation :: IsNSData data_ => data_ -> IO (Id NSCharacterSet)
characterSetWithBitmapRepresentation data_ =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' characterSetWithBitmapRepresentationSelector (toNSData data_)

-- | @+ characterSetWithContentsOfFile:@
characterSetWithContentsOfFile :: IsNSString fName => fName -> IO (Id NSCharacterSet)
characterSetWithContentsOfFile fName =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' characterSetWithContentsOfFileSelector (toNSString fName)

-- | @- initWithCoder:@
initWithCoder :: (IsNSCharacterSet nsCharacterSet, IsNSCoder coder) => nsCharacterSet -> coder -> IO (Id NSCharacterSet)
initWithCoder nsCharacterSet coder =
  sendOwnedMessage nsCharacterSet initWithCoderSelector (toNSCoder coder)

-- | @- characterIsMember:@
characterIsMember :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> CUShort -> IO Bool
characterIsMember nsCharacterSet aCharacter =
  sendMessage nsCharacterSet characterIsMemberSelector aCharacter

-- | @- longCharacterIsMember:@
longCharacterIsMember :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> CUInt -> IO Bool
longCharacterIsMember nsCharacterSet theLongChar =
  sendMessage nsCharacterSet longCharacterIsMemberSelector theLongChar

-- | @- isSupersetOfSet:@
isSupersetOfSet :: (IsNSCharacterSet nsCharacterSet, IsNSCharacterSet theOtherSet) => nsCharacterSet -> theOtherSet -> IO Bool
isSupersetOfSet nsCharacterSet theOtherSet =
  sendMessage nsCharacterSet isSupersetOfSetSelector (toNSCharacterSet theOtherSet)

-- | @- hasMemberInPlane:@
hasMemberInPlane :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> CUChar -> IO Bool
hasMemberInPlane nsCharacterSet thePlane =
  sendMessage nsCharacterSet hasMemberInPlaneSelector thePlane

-- | @+ controlCharacterSet@
controlCharacterSet :: IO (Id NSCharacterSet)
controlCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' controlCharacterSetSelector

-- | @+ whitespaceCharacterSet@
whitespaceCharacterSet :: IO (Id NSCharacterSet)
whitespaceCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' whitespaceCharacterSetSelector

-- | @+ whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSet :: IO (Id NSCharacterSet)
whitespaceAndNewlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' whitespaceAndNewlineCharacterSetSelector

-- | @+ decimalDigitCharacterSet@
decimalDigitCharacterSet :: IO (Id NSCharacterSet)
decimalDigitCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' decimalDigitCharacterSetSelector

-- | @+ letterCharacterSet@
letterCharacterSet :: IO (Id NSCharacterSet)
letterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' letterCharacterSetSelector

-- | @+ lowercaseLetterCharacterSet@
lowercaseLetterCharacterSet :: IO (Id NSCharacterSet)
lowercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' lowercaseLetterCharacterSetSelector

-- | @+ uppercaseLetterCharacterSet@
uppercaseLetterCharacterSet :: IO (Id NSCharacterSet)
uppercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' uppercaseLetterCharacterSetSelector

-- | @+ nonBaseCharacterSet@
nonBaseCharacterSet :: IO (Id NSCharacterSet)
nonBaseCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' nonBaseCharacterSetSelector

-- | @+ alphanumericCharacterSet@
alphanumericCharacterSet :: IO (Id NSCharacterSet)
alphanumericCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' alphanumericCharacterSetSelector

-- | @+ decomposableCharacterSet@
decomposableCharacterSet :: IO (Id NSCharacterSet)
decomposableCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' decomposableCharacterSetSelector

-- | @+ illegalCharacterSet@
illegalCharacterSet :: IO (Id NSCharacterSet)
illegalCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' illegalCharacterSetSelector

-- | @+ punctuationCharacterSet@
punctuationCharacterSet :: IO (Id NSCharacterSet)
punctuationCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' punctuationCharacterSetSelector

-- | @+ capitalizedLetterCharacterSet@
capitalizedLetterCharacterSet :: IO (Id NSCharacterSet)
capitalizedLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' capitalizedLetterCharacterSetSelector

-- | @+ symbolCharacterSet@
symbolCharacterSet :: IO (Id NSCharacterSet)
symbolCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' symbolCharacterSetSelector

-- | @+ newlineCharacterSet@
newlineCharacterSet :: IO (Id NSCharacterSet)
newlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendOwnedClassMessage cls' newlineCharacterSetSelector

-- | @- bitmapRepresentation@
bitmapRepresentation :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> IO (Id NSData)
bitmapRepresentation nsCharacterSet =
  sendMessage nsCharacterSet bitmapRepresentationSelector

-- | @- invertedSet@
invertedSet :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> IO (Id NSCharacterSet)
invertedSet nsCharacterSet =
  sendMessage nsCharacterSet invertedSetSelector

-- | @+ URLUserAllowedCharacterSet@
urlUserAllowedCharacterSet :: IO (Id NSCharacterSet)
urlUserAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' urlUserAllowedCharacterSetSelector

-- | @+ URLPasswordAllowedCharacterSet@
urlPasswordAllowedCharacterSet :: IO (Id NSCharacterSet)
urlPasswordAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' urlPasswordAllowedCharacterSetSelector

-- | @+ URLHostAllowedCharacterSet@
urlHostAllowedCharacterSet :: IO (Id NSCharacterSet)
urlHostAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' urlHostAllowedCharacterSetSelector

-- | @+ URLPathAllowedCharacterSet@
urlPathAllowedCharacterSet :: IO (Id NSCharacterSet)
urlPathAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' urlPathAllowedCharacterSetSelector

-- | @+ URLQueryAllowedCharacterSet@
urlQueryAllowedCharacterSet :: IO (Id NSCharacterSet)
urlQueryAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' urlQueryAllowedCharacterSetSelector

-- | @+ URLFragmentAllowedCharacterSet@
urlFragmentAllowedCharacterSet :: IO (Id NSCharacterSet)
urlFragmentAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMessage cls' urlFragmentAllowedCharacterSetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @characterSetWithRange:@
characterSetWithRangeSelector :: Selector '[NSRange] (Id NSCharacterSet)
characterSetWithRangeSelector = mkSelector "characterSetWithRange:"

-- | @Selector@ for @characterSetWithCharactersInString:@
characterSetWithCharactersInStringSelector :: Selector '[Id NSString] (Id NSCharacterSet)
characterSetWithCharactersInStringSelector = mkSelector "characterSetWithCharactersInString:"

-- | @Selector@ for @characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentationSelector :: Selector '[Id NSData] (Id NSCharacterSet)
characterSetWithBitmapRepresentationSelector = mkSelector "characterSetWithBitmapRepresentation:"

-- | @Selector@ for @characterSetWithContentsOfFile:@
characterSetWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSCharacterSet)
characterSetWithContentsOfFileSelector = mkSelector "characterSetWithContentsOfFile:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSCharacterSet)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @characterIsMember:@
characterIsMemberSelector :: Selector '[CUShort] Bool
characterIsMemberSelector = mkSelector "characterIsMember:"

-- | @Selector@ for @longCharacterIsMember:@
longCharacterIsMemberSelector :: Selector '[CUInt] Bool
longCharacterIsMemberSelector = mkSelector "longCharacterIsMember:"

-- | @Selector@ for @isSupersetOfSet:@
isSupersetOfSetSelector :: Selector '[Id NSCharacterSet] Bool
isSupersetOfSetSelector = mkSelector "isSupersetOfSet:"

-- | @Selector@ for @hasMemberInPlane:@
hasMemberInPlaneSelector :: Selector '[CUChar] Bool
hasMemberInPlaneSelector = mkSelector "hasMemberInPlane:"

-- | @Selector@ for @controlCharacterSet@
controlCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
controlCharacterSetSelector = mkSelector "controlCharacterSet"

-- | @Selector@ for @whitespaceCharacterSet@
whitespaceCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
whitespaceCharacterSetSelector = mkSelector "whitespaceCharacterSet"

-- | @Selector@ for @whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
whitespaceAndNewlineCharacterSetSelector = mkSelector "whitespaceAndNewlineCharacterSet"

-- | @Selector@ for @decimalDigitCharacterSet@
decimalDigitCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
decimalDigitCharacterSetSelector = mkSelector "decimalDigitCharacterSet"

-- | @Selector@ for @letterCharacterSet@
letterCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
letterCharacterSetSelector = mkSelector "letterCharacterSet"

-- | @Selector@ for @lowercaseLetterCharacterSet@
lowercaseLetterCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
lowercaseLetterCharacterSetSelector = mkSelector "lowercaseLetterCharacterSet"

-- | @Selector@ for @uppercaseLetterCharacterSet@
uppercaseLetterCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
uppercaseLetterCharacterSetSelector = mkSelector "uppercaseLetterCharacterSet"

-- | @Selector@ for @nonBaseCharacterSet@
nonBaseCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
nonBaseCharacterSetSelector = mkSelector "nonBaseCharacterSet"

-- | @Selector@ for @alphanumericCharacterSet@
alphanumericCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
alphanumericCharacterSetSelector = mkSelector "alphanumericCharacterSet"

-- | @Selector@ for @decomposableCharacterSet@
decomposableCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
decomposableCharacterSetSelector = mkSelector "decomposableCharacterSet"

-- | @Selector@ for @illegalCharacterSet@
illegalCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
illegalCharacterSetSelector = mkSelector "illegalCharacterSet"

-- | @Selector@ for @punctuationCharacterSet@
punctuationCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
punctuationCharacterSetSelector = mkSelector "punctuationCharacterSet"

-- | @Selector@ for @capitalizedLetterCharacterSet@
capitalizedLetterCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
capitalizedLetterCharacterSetSelector = mkSelector "capitalizedLetterCharacterSet"

-- | @Selector@ for @symbolCharacterSet@
symbolCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
symbolCharacterSetSelector = mkSelector "symbolCharacterSet"

-- | @Selector@ for @newlineCharacterSet@
newlineCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
newlineCharacterSetSelector = mkSelector "newlineCharacterSet"

-- | @Selector@ for @bitmapRepresentation@
bitmapRepresentationSelector :: Selector '[] (Id NSData)
bitmapRepresentationSelector = mkSelector "bitmapRepresentation"

-- | @Selector@ for @invertedSet@
invertedSetSelector :: Selector '[] (Id NSCharacterSet)
invertedSetSelector = mkSelector "invertedSet"

-- | @Selector@ for @URLUserAllowedCharacterSet@
urlUserAllowedCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
urlUserAllowedCharacterSetSelector = mkSelector "URLUserAllowedCharacterSet"

-- | @Selector@ for @URLPasswordAllowedCharacterSet@
urlPasswordAllowedCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
urlPasswordAllowedCharacterSetSelector = mkSelector "URLPasswordAllowedCharacterSet"

-- | @Selector@ for @URLHostAllowedCharacterSet@
urlHostAllowedCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
urlHostAllowedCharacterSetSelector = mkSelector "URLHostAllowedCharacterSet"

-- | @Selector@ for @URLPathAllowedCharacterSet@
urlPathAllowedCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
urlPathAllowedCharacterSetSelector = mkSelector "URLPathAllowedCharacterSet"

-- | @Selector@ for @URLQueryAllowedCharacterSet@
urlQueryAllowedCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
urlQueryAllowedCharacterSetSelector = mkSelector "URLQueryAllowedCharacterSet"

-- | @Selector@ for @URLFragmentAllowedCharacterSet@
urlFragmentAllowedCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
urlFragmentAllowedCharacterSetSelector = mkSelector "URLFragmentAllowedCharacterSet"

