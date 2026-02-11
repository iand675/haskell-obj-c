{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScanner@.
module ObjC.Foundation.NSScanner
  ( NSScanner
  , IsNSScanner(..)
  , initWithString
  , scanDecimal
  , scanInt
  , scanInteger
  , scanLongLong
  , scanUnsignedLongLong
  , scanFloat
  , scanDouble
  , scanHexInt
  , scanHexLongLong
  , scanHexFloat
  , scanHexDouble
  , scanString_intoString
  , scanCharactersFromSet_intoString
  , scanUpToString_intoString
  , scanUpToCharactersFromSet_intoString
  , scannerWithString
  , localizedScannerWithString
  , string
  , scanLocation
  , setScanLocation
  , charactersToBeSkipped
  , setCharactersToBeSkipped
  , caseSensitive
  , setCaseSensitive
  , locale
  , setLocale
  , atEnd
  , initWithStringSelector
  , scanDecimalSelector
  , scanIntSelector
  , scanIntegerSelector
  , scanLongLongSelector
  , scanUnsignedLongLongSelector
  , scanFloatSelector
  , scanDoubleSelector
  , scanHexIntSelector
  , scanHexLongLongSelector
  , scanHexFloatSelector
  , scanHexDoubleSelector
  , scanString_intoStringSelector
  , scanCharactersFromSet_intoStringSelector
  , scanUpToString_intoStringSelector
  , scanUpToCharactersFromSet_intoStringSelector
  , scannerWithStringSelector
  , localizedScannerWithStringSelector
  , stringSelector
  , scanLocationSelector
  , setScanLocationSelector
  , charactersToBeSkippedSelector
  , setCharactersToBeSkippedSelector
  , caseSensitiveSelector
  , setCaseSensitiveSelector
  , localeSelector
  , setLocaleSelector
  , atEndSelector


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

-- | @- initWithString:@
initWithString :: (IsNSScanner nsScanner, IsNSString string) => nsScanner -> string -> IO (Id NSScanner)
initWithString nsScanner  string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsScanner (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- scanDecimal:@
scanDecimal :: IsNSScanner nsScanner => nsScanner -> RawId -> IO Bool
scanDecimal nsScanner  dcm =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanDecimal:") retCULong [argPtr (castPtr (unRawId dcm) :: Ptr ())]

-- | @- scanInt:@
scanInt :: IsNSScanner nsScanner => nsScanner -> Ptr CInt -> IO Bool
scanInt nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanInt:") retCULong [argPtr result]

-- | @- scanInteger:@
scanInteger :: IsNSScanner nsScanner => nsScanner -> Ptr CLong -> IO Bool
scanInteger nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanInteger:") retCULong [argPtr result]

-- | @- scanLongLong:@
scanLongLong :: IsNSScanner nsScanner => nsScanner -> Ptr CLong -> IO Bool
scanLongLong nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanLongLong:") retCULong [argPtr result]

-- | @- scanUnsignedLongLong:@
scanUnsignedLongLong :: IsNSScanner nsScanner => nsScanner -> Ptr CULong -> IO Bool
scanUnsignedLongLong nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanUnsignedLongLong:") retCULong [argPtr result]

-- | @- scanFloat:@
scanFloat :: IsNSScanner nsScanner => nsScanner -> Ptr CFloat -> IO Bool
scanFloat nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanFloat:") retCULong [argPtr result]

-- | @- scanDouble:@
scanDouble :: IsNSScanner nsScanner => nsScanner -> Ptr CDouble -> IO Bool
scanDouble nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanDouble:") retCULong [argPtr result]

-- | @- scanHexInt:@
scanHexInt :: IsNSScanner nsScanner => nsScanner -> Ptr CUInt -> IO Bool
scanHexInt nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanHexInt:") retCULong [argPtr result]

-- | @- scanHexLongLong:@
scanHexLongLong :: IsNSScanner nsScanner => nsScanner -> Ptr CULong -> IO Bool
scanHexLongLong nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanHexLongLong:") retCULong [argPtr result]

-- | @- scanHexFloat:@
scanHexFloat :: IsNSScanner nsScanner => nsScanner -> Ptr CFloat -> IO Bool
scanHexFloat nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanHexFloat:") retCULong [argPtr result]

-- | @- scanHexDouble:@
scanHexDouble :: IsNSScanner nsScanner => nsScanner -> Ptr CDouble -> IO Bool
scanHexDouble nsScanner  result =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanHexDouble:") retCULong [argPtr result]

-- | @- scanString:intoString:@
scanString_intoString :: (IsNSScanner nsScanner, IsNSString string, IsNSString result) => nsScanner -> string -> result -> IO Bool
scanString_intoString nsScanner  string result =
  withObjCPtr string $ \raw_string ->
    withObjCPtr result $ \raw_result ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanString:intoString:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_result :: Ptr ())]

-- | @- scanCharactersFromSet:intoString:@
scanCharactersFromSet_intoString :: (IsNSScanner nsScanner, IsNSCharacterSet set, IsNSString result) => nsScanner -> set -> result -> IO Bool
scanCharactersFromSet_intoString nsScanner  set result =
  withObjCPtr set $ \raw_set ->
    withObjCPtr result $ \raw_result ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanCharactersFromSet:intoString:") retCULong [argPtr (castPtr raw_set :: Ptr ()), argPtr (castPtr raw_result :: Ptr ())]

-- | @- scanUpToString:intoString:@
scanUpToString_intoString :: (IsNSScanner nsScanner, IsNSString string, IsNSString result) => nsScanner -> string -> result -> IO Bool
scanUpToString_intoString nsScanner  string result =
  withObjCPtr string $ \raw_string ->
    withObjCPtr result $ \raw_result ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanUpToString:intoString:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_result :: Ptr ())]

-- | @- scanUpToCharactersFromSet:intoString:@
scanUpToCharactersFromSet_intoString :: (IsNSScanner nsScanner, IsNSCharacterSet set, IsNSString result) => nsScanner -> set -> result -> IO Bool
scanUpToCharactersFromSet_intoString nsScanner  set result =
  withObjCPtr set $ \raw_set ->
    withObjCPtr result $ \raw_result ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "scanUpToCharactersFromSet:intoString:") retCULong [argPtr (castPtr raw_set :: Ptr ()), argPtr (castPtr raw_result :: Ptr ())]

-- | @+ scannerWithString:@
scannerWithString :: IsNSString string => string -> IO (Id NSScanner)
scannerWithString string =
  do
    cls' <- getRequiredClass "NSScanner"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "scannerWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localizedScannerWithString:@
localizedScannerWithString :: IsNSString string => string -> IO RawId
localizedScannerWithString string =
  do
    cls' <- getRequiredClass "NSScanner"
    withObjCPtr string $ \raw_string ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "localizedScannerWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())]

-- | @- string@
string :: IsNSScanner nsScanner => nsScanner -> IO (Id NSString)
string nsScanner  =
    sendMsg nsScanner (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scanLocation@
scanLocation :: IsNSScanner nsScanner => nsScanner -> IO CULong
scanLocation nsScanner  =
    sendMsg nsScanner (mkSelector "scanLocation") retCULong []

-- | @- setScanLocation:@
setScanLocation :: IsNSScanner nsScanner => nsScanner -> CULong -> IO ()
setScanLocation nsScanner  value =
    sendMsg nsScanner (mkSelector "setScanLocation:") retVoid [argCULong value]

-- | @- charactersToBeSkipped@
charactersToBeSkipped :: IsNSScanner nsScanner => nsScanner -> IO (Id NSCharacterSet)
charactersToBeSkipped nsScanner  =
    sendMsg nsScanner (mkSelector "charactersToBeSkipped") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharactersToBeSkipped:@
setCharactersToBeSkipped :: (IsNSScanner nsScanner, IsNSCharacterSet value) => nsScanner -> value -> IO ()
setCharactersToBeSkipped nsScanner  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsScanner (mkSelector "setCharactersToBeSkipped:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caseSensitive@
caseSensitive :: IsNSScanner nsScanner => nsScanner -> IO Bool
caseSensitive nsScanner  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "caseSensitive") retCULong []

-- | @- setCaseSensitive:@
setCaseSensitive :: IsNSScanner nsScanner => nsScanner -> Bool -> IO ()
setCaseSensitive nsScanner  value =
    sendMsg nsScanner (mkSelector "setCaseSensitive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- locale@
locale :: IsNSScanner nsScanner => nsScanner -> IO RawId
locale nsScanner  =
    fmap (RawId . castPtr) $ sendMsg nsScanner (mkSelector "locale") (retPtr retVoid) []

-- | @- setLocale:@
setLocale :: IsNSScanner nsScanner => nsScanner -> RawId -> IO ()
setLocale nsScanner  value =
    sendMsg nsScanner (mkSelector "setLocale:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- atEnd@
atEnd :: IsNSScanner nsScanner => nsScanner -> IO Bool
atEnd nsScanner  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScanner (mkSelector "atEnd") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @scanDecimal:@
scanDecimalSelector :: Selector
scanDecimalSelector = mkSelector "scanDecimal:"

-- | @Selector@ for @scanInt:@
scanIntSelector :: Selector
scanIntSelector = mkSelector "scanInt:"

-- | @Selector@ for @scanInteger:@
scanIntegerSelector :: Selector
scanIntegerSelector = mkSelector "scanInteger:"

-- | @Selector@ for @scanLongLong:@
scanLongLongSelector :: Selector
scanLongLongSelector = mkSelector "scanLongLong:"

-- | @Selector@ for @scanUnsignedLongLong:@
scanUnsignedLongLongSelector :: Selector
scanUnsignedLongLongSelector = mkSelector "scanUnsignedLongLong:"

-- | @Selector@ for @scanFloat:@
scanFloatSelector :: Selector
scanFloatSelector = mkSelector "scanFloat:"

-- | @Selector@ for @scanDouble:@
scanDoubleSelector :: Selector
scanDoubleSelector = mkSelector "scanDouble:"

-- | @Selector@ for @scanHexInt:@
scanHexIntSelector :: Selector
scanHexIntSelector = mkSelector "scanHexInt:"

-- | @Selector@ for @scanHexLongLong:@
scanHexLongLongSelector :: Selector
scanHexLongLongSelector = mkSelector "scanHexLongLong:"

-- | @Selector@ for @scanHexFloat:@
scanHexFloatSelector :: Selector
scanHexFloatSelector = mkSelector "scanHexFloat:"

-- | @Selector@ for @scanHexDouble:@
scanHexDoubleSelector :: Selector
scanHexDoubleSelector = mkSelector "scanHexDouble:"

-- | @Selector@ for @scanString:intoString:@
scanString_intoStringSelector :: Selector
scanString_intoStringSelector = mkSelector "scanString:intoString:"

-- | @Selector@ for @scanCharactersFromSet:intoString:@
scanCharactersFromSet_intoStringSelector :: Selector
scanCharactersFromSet_intoStringSelector = mkSelector "scanCharactersFromSet:intoString:"

-- | @Selector@ for @scanUpToString:intoString:@
scanUpToString_intoStringSelector :: Selector
scanUpToString_intoStringSelector = mkSelector "scanUpToString:intoString:"

-- | @Selector@ for @scanUpToCharactersFromSet:intoString:@
scanUpToCharactersFromSet_intoStringSelector :: Selector
scanUpToCharactersFromSet_intoStringSelector = mkSelector "scanUpToCharactersFromSet:intoString:"

-- | @Selector@ for @scannerWithString:@
scannerWithStringSelector :: Selector
scannerWithStringSelector = mkSelector "scannerWithString:"

-- | @Selector@ for @localizedScannerWithString:@
localizedScannerWithStringSelector :: Selector
localizedScannerWithStringSelector = mkSelector "localizedScannerWithString:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @scanLocation@
scanLocationSelector :: Selector
scanLocationSelector = mkSelector "scanLocation"

-- | @Selector@ for @setScanLocation:@
setScanLocationSelector :: Selector
setScanLocationSelector = mkSelector "setScanLocation:"

-- | @Selector@ for @charactersToBeSkipped@
charactersToBeSkippedSelector :: Selector
charactersToBeSkippedSelector = mkSelector "charactersToBeSkipped"

-- | @Selector@ for @setCharactersToBeSkipped:@
setCharactersToBeSkippedSelector :: Selector
setCharactersToBeSkippedSelector = mkSelector "setCharactersToBeSkipped:"

-- | @Selector@ for @caseSensitive@
caseSensitiveSelector :: Selector
caseSensitiveSelector = mkSelector "caseSensitive"

-- | @Selector@ for @setCaseSensitive:@
setCaseSensitiveSelector :: Selector
setCaseSensitiveSelector = mkSelector "setCaseSensitive:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @atEnd@
atEndSelector :: Selector
atEndSelector = mkSelector "atEnd"

