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
  , characterSetWithRangeSelector
  , characterSetWithCharactersInStringSelector
  , characterSetWithBitmapRepresentationSelector
  , characterSetWithContentsOfFileSelector
  , initWithCoderSelector
  , characterIsMemberSelector
  , longCharacterIsMemberSelector
  , isSupersetOfSetSelector
  , hasMemberInPlaneSelector
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
  , bitmapRepresentationSelector
  , invertedSetSelector
  , urlUserAllowedCharacterSetSelector
  , urlPasswordAllowedCharacterSetSelector
  , urlHostAllowedCharacterSetSelector
  , urlPathAllowedCharacterSetSelector
  , urlQueryAllowedCharacterSetSelector
  , urlFragmentAllowedCharacterSetSelector


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

-- | @+ characterSetWithRange:@
characterSetWithRange :: NSRange -> IO (Id NSCharacterSet)
characterSetWithRange aRange =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "characterSetWithRange:") (retPtr retVoid) [argNSRange aRange] >>= retainedObject . castPtr

-- | @+ characterSetWithCharactersInString:@
characterSetWithCharactersInString :: IsNSString aString => aString -> IO (Id NSCharacterSet)
characterSetWithCharactersInString aString =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    withObjCPtr aString $ \raw_aString ->
      sendClassMsg cls' (mkSelector "characterSetWithCharactersInString:") (retPtr retVoid) [argPtr (castPtr raw_aString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ characterSetWithBitmapRepresentation:@
characterSetWithBitmapRepresentation :: IsNSData data_ => data_ -> IO (Id NSCharacterSet)
characterSetWithBitmapRepresentation data_ =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "characterSetWithBitmapRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ characterSetWithContentsOfFile:@
characterSetWithContentsOfFile :: IsNSString fName => fName -> IO (Id NSCharacterSet)
characterSetWithContentsOfFile fName =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    withObjCPtr fName $ \raw_fName ->
      sendClassMsg cls' (mkSelector "characterSetWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_fName :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSCharacterSet nsCharacterSet, IsNSCoder coder) => nsCharacterSet -> coder -> IO (Id NSCharacterSet)
initWithCoder nsCharacterSet  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsCharacterSet (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- characterIsMember:@
characterIsMember :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> CUShort -> IO Bool
characterIsMember nsCharacterSet  aCharacter =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCharacterSet (mkSelector "characterIsMember:") retCULong [argCUInt (fromIntegral aCharacter)]

-- | @- longCharacterIsMember:@
longCharacterIsMember :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> CUInt -> IO Bool
longCharacterIsMember nsCharacterSet  theLongChar =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCharacterSet (mkSelector "longCharacterIsMember:") retCULong [argCUInt (fromIntegral theLongChar)]

-- | @- isSupersetOfSet:@
isSupersetOfSet :: (IsNSCharacterSet nsCharacterSet, IsNSCharacterSet theOtherSet) => nsCharacterSet -> theOtherSet -> IO Bool
isSupersetOfSet nsCharacterSet  theOtherSet =
withObjCPtr theOtherSet $ \raw_theOtherSet ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCharacterSet (mkSelector "isSupersetOfSet:") retCULong [argPtr (castPtr raw_theOtherSet :: Ptr ())]

-- | @- hasMemberInPlane:@
hasMemberInPlane :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> CUChar -> IO Bool
hasMemberInPlane nsCharacterSet  thePlane =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCharacterSet (mkSelector "hasMemberInPlane:") retCULong [argCUChar (fromIntegral thePlane)]

-- | @+ controlCharacterSet@
controlCharacterSet :: IO (Id NSCharacterSet)
controlCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "controlCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ whitespaceCharacterSet@
whitespaceCharacterSet :: IO (Id NSCharacterSet)
whitespaceCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "whitespaceCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ whitespaceAndNewlineCharacterSet@
whitespaceAndNewlineCharacterSet :: IO (Id NSCharacterSet)
whitespaceAndNewlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "whitespaceAndNewlineCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decimalDigitCharacterSet@
decimalDigitCharacterSet :: IO (Id NSCharacterSet)
decimalDigitCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "decimalDigitCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ letterCharacterSet@
letterCharacterSet :: IO (Id NSCharacterSet)
letterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "letterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ lowercaseLetterCharacterSet@
lowercaseLetterCharacterSet :: IO (Id NSCharacterSet)
lowercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "lowercaseLetterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ uppercaseLetterCharacterSet@
uppercaseLetterCharacterSet :: IO (Id NSCharacterSet)
uppercaseLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "uppercaseLetterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nonBaseCharacterSet@
nonBaseCharacterSet :: IO (Id NSCharacterSet)
nonBaseCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "nonBaseCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ alphanumericCharacterSet@
alphanumericCharacterSet :: IO (Id NSCharacterSet)
alphanumericCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "alphanumericCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decomposableCharacterSet@
decomposableCharacterSet :: IO (Id NSCharacterSet)
decomposableCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "decomposableCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ illegalCharacterSet@
illegalCharacterSet :: IO (Id NSCharacterSet)
illegalCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "illegalCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ punctuationCharacterSet@
punctuationCharacterSet :: IO (Id NSCharacterSet)
punctuationCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "punctuationCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ capitalizedLetterCharacterSet@
capitalizedLetterCharacterSet :: IO (Id NSCharacterSet)
capitalizedLetterCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "capitalizedLetterCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ symbolCharacterSet@
symbolCharacterSet :: IO (Id NSCharacterSet)
symbolCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "symbolCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ newlineCharacterSet@
newlineCharacterSet :: IO (Id NSCharacterSet)
newlineCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "newlineCharacterSet") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- bitmapRepresentation@
bitmapRepresentation :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> IO (Id NSData)
bitmapRepresentation nsCharacterSet  =
  sendMsg nsCharacterSet (mkSelector "bitmapRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- invertedSet@
invertedSet :: IsNSCharacterSet nsCharacterSet => nsCharacterSet -> IO (Id NSCharacterSet)
invertedSet nsCharacterSet  =
  sendMsg nsCharacterSet (mkSelector "invertedSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ URLUserAllowedCharacterSet@
urlUserAllowedCharacterSet :: IO (Id NSCharacterSet)
urlUserAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "URLUserAllowedCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ URLPasswordAllowedCharacterSet@
urlPasswordAllowedCharacterSet :: IO (Id NSCharacterSet)
urlPasswordAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "URLPasswordAllowedCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ URLHostAllowedCharacterSet@
urlHostAllowedCharacterSet :: IO (Id NSCharacterSet)
urlHostAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "URLHostAllowedCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ URLPathAllowedCharacterSet@
urlPathAllowedCharacterSet :: IO (Id NSCharacterSet)
urlPathAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "URLPathAllowedCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ URLQueryAllowedCharacterSet@
urlQueryAllowedCharacterSet :: IO (Id NSCharacterSet)
urlQueryAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "URLQueryAllowedCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ URLFragmentAllowedCharacterSet@
urlFragmentAllowedCharacterSet :: IO (Id NSCharacterSet)
urlFragmentAllowedCharacterSet  =
  do
    cls' <- getRequiredClass "NSCharacterSet"
    sendClassMsg cls' (mkSelector "URLFragmentAllowedCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @characterIsMember:@
characterIsMemberSelector :: Selector
characterIsMemberSelector = mkSelector "characterIsMember:"

-- | @Selector@ for @longCharacterIsMember:@
longCharacterIsMemberSelector :: Selector
longCharacterIsMemberSelector = mkSelector "longCharacterIsMember:"

-- | @Selector@ for @isSupersetOfSet:@
isSupersetOfSetSelector :: Selector
isSupersetOfSetSelector = mkSelector "isSupersetOfSet:"

-- | @Selector@ for @hasMemberInPlane:@
hasMemberInPlaneSelector :: Selector
hasMemberInPlaneSelector = mkSelector "hasMemberInPlane:"

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

-- | @Selector@ for @bitmapRepresentation@
bitmapRepresentationSelector :: Selector
bitmapRepresentationSelector = mkSelector "bitmapRepresentation"

-- | @Selector@ for @invertedSet@
invertedSetSelector :: Selector
invertedSetSelector = mkSelector "invertedSet"

-- | @Selector@ for @URLUserAllowedCharacterSet@
urlUserAllowedCharacterSetSelector :: Selector
urlUserAllowedCharacterSetSelector = mkSelector "URLUserAllowedCharacterSet"

-- | @Selector@ for @URLPasswordAllowedCharacterSet@
urlPasswordAllowedCharacterSetSelector :: Selector
urlPasswordAllowedCharacterSetSelector = mkSelector "URLPasswordAllowedCharacterSet"

-- | @Selector@ for @URLHostAllowedCharacterSet@
urlHostAllowedCharacterSetSelector :: Selector
urlHostAllowedCharacterSetSelector = mkSelector "URLHostAllowedCharacterSet"

-- | @Selector@ for @URLPathAllowedCharacterSet@
urlPathAllowedCharacterSetSelector :: Selector
urlPathAllowedCharacterSetSelector = mkSelector "URLPathAllowedCharacterSet"

-- | @Selector@ for @URLQueryAllowedCharacterSet@
urlQueryAllowedCharacterSetSelector :: Selector
urlQueryAllowedCharacterSetSelector = mkSelector "URLQueryAllowedCharacterSet"

-- | @Selector@ for @URLFragmentAllowedCharacterSet@
urlFragmentAllowedCharacterSetSelector :: Selector
urlFragmentAllowedCharacterSetSelector = mkSelector "URLFragmentAllowedCharacterSet"

