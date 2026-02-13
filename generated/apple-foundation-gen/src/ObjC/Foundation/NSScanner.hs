{-# LANGUAGE DataKinds #-}
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
  , atEndSelector
  , caseSensitiveSelector
  , charactersToBeSkippedSelector
  , initWithStringSelector
  , localeSelector
  , localizedScannerWithStringSelector
  , scanCharactersFromSet_intoStringSelector
  , scanDecimalSelector
  , scanDoubleSelector
  , scanFloatSelector
  , scanHexDoubleSelector
  , scanHexFloatSelector
  , scanHexIntSelector
  , scanHexLongLongSelector
  , scanIntSelector
  , scanIntegerSelector
  , scanLocationSelector
  , scanLongLongSelector
  , scanString_intoStringSelector
  , scanUnsignedLongLongSelector
  , scanUpToCharactersFromSet_intoStringSelector
  , scanUpToString_intoStringSelector
  , scannerWithStringSelector
  , setCaseSensitiveSelector
  , setCharactersToBeSkippedSelector
  , setLocaleSelector
  , setScanLocationSelector
  , stringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithString:@
initWithString :: (IsNSScanner nsScanner, IsNSString string) => nsScanner -> string -> IO (Id NSScanner)
initWithString nsScanner string =
  sendOwnedMessage nsScanner initWithStringSelector (toNSString string)

-- | @- scanDecimal:@
scanDecimal :: IsNSScanner nsScanner => nsScanner -> RawId -> IO Bool
scanDecimal nsScanner dcm =
  sendMessage nsScanner scanDecimalSelector dcm

-- | @- scanInt:@
scanInt :: IsNSScanner nsScanner => nsScanner -> Ptr CInt -> IO Bool
scanInt nsScanner result =
  sendMessage nsScanner scanIntSelector result

-- | @- scanInteger:@
scanInteger :: IsNSScanner nsScanner => nsScanner -> Ptr CLong -> IO Bool
scanInteger nsScanner result =
  sendMessage nsScanner scanIntegerSelector result

-- | @- scanLongLong:@
scanLongLong :: IsNSScanner nsScanner => nsScanner -> Ptr CLong -> IO Bool
scanLongLong nsScanner result =
  sendMessage nsScanner scanLongLongSelector result

-- | @- scanUnsignedLongLong:@
scanUnsignedLongLong :: IsNSScanner nsScanner => nsScanner -> Ptr CULong -> IO Bool
scanUnsignedLongLong nsScanner result =
  sendMessage nsScanner scanUnsignedLongLongSelector result

-- | @- scanFloat:@
scanFloat :: IsNSScanner nsScanner => nsScanner -> Ptr CFloat -> IO Bool
scanFloat nsScanner result =
  sendMessage nsScanner scanFloatSelector result

-- | @- scanDouble:@
scanDouble :: IsNSScanner nsScanner => nsScanner -> Ptr CDouble -> IO Bool
scanDouble nsScanner result =
  sendMessage nsScanner scanDoubleSelector result

-- | @- scanHexInt:@
scanHexInt :: IsNSScanner nsScanner => nsScanner -> Ptr CUInt -> IO Bool
scanHexInt nsScanner result =
  sendMessage nsScanner scanHexIntSelector result

-- | @- scanHexLongLong:@
scanHexLongLong :: IsNSScanner nsScanner => nsScanner -> Ptr CULong -> IO Bool
scanHexLongLong nsScanner result =
  sendMessage nsScanner scanHexLongLongSelector result

-- | @- scanHexFloat:@
scanHexFloat :: IsNSScanner nsScanner => nsScanner -> Ptr CFloat -> IO Bool
scanHexFloat nsScanner result =
  sendMessage nsScanner scanHexFloatSelector result

-- | @- scanHexDouble:@
scanHexDouble :: IsNSScanner nsScanner => nsScanner -> Ptr CDouble -> IO Bool
scanHexDouble nsScanner result =
  sendMessage nsScanner scanHexDoubleSelector result

-- | @- scanString:intoString:@
scanString_intoString :: (IsNSScanner nsScanner, IsNSString string, IsNSString result) => nsScanner -> string -> result -> IO Bool
scanString_intoString nsScanner string result =
  sendMessage nsScanner scanString_intoStringSelector (toNSString string) (toNSString result)

-- | @- scanCharactersFromSet:intoString:@
scanCharactersFromSet_intoString :: (IsNSScanner nsScanner, IsNSCharacterSet set, IsNSString result) => nsScanner -> set -> result -> IO Bool
scanCharactersFromSet_intoString nsScanner set result =
  sendMessage nsScanner scanCharactersFromSet_intoStringSelector (toNSCharacterSet set) (toNSString result)

-- | @- scanUpToString:intoString:@
scanUpToString_intoString :: (IsNSScanner nsScanner, IsNSString string, IsNSString result) => nsScanner -> string -> result -> IO Bool
scanUpToString_intoString nsScanner string result =
  sendMessage nsScanner scanUpToString_intoStringSelector (toNSString string) (toNSString result)

-- | @- scanUpToCharactersFromSet:intoString:@
scanUpToCharactersFromSet_intoString :: (IsNSScanner nsScanner, IsNSCharacterSet set, IsNSString result) => nsScanner -> set -> result -> IO Bool
scanUpToCharactersFromSet_intoString nsScanner set result =
  sendMessage nsScanner scanUpToCharactersFromSet_intoStringSelector (toNSCharacterSet set) (toNSString result)

-- | @+ scannerWithString:@
scannerWithString :: IsNSString string => string -> IO (Id NSScanner)
scannerWithString string =
  do
    cls' <- getRequiredClass "NSScanner"
    sendClassMessage cls' scannerWithStringSelector (toNSString string)

-- | @+ localizedScannerWithString:@
localizedScannerWithString :: IsNSString string => string -> IO RawId
localizedScannerWithString string =
  do
    cls' <- getRequiredClass "NSScanner"
    sendClassMessage cls' localizedScannerWithStringSelector (toNSString string)

-- | @- string@
string :: IsNSScanner nsScanner => nsScanner -> IO (Id NSString)
string nsScanner =
  sendMessage nsScanner stringSelector

-- | @- scanLocation@
scanLocation :: IsNSScanner nsScanner => nsScanner -> IO CULong
scanLocation nsScanner =
  sendMessage nsScanner scanLocationSelector

-- | @- setScanLocation:@
setScanLocation :: IsNSScanner nsScanner => nsScanner -> CULong -> IO ()
setScanLocation nsScanner value =
  sendMessage nsScanner setScanLocationSelector value

-- | @- charactersToBeSkipped@
charactersToBeSkipped :: IsNSScanner nsScanner => nsScanner -> IO (Id NSCharacterSet)
charactersToBeSkipped nsScanner =
  sendMessage nsScanner charactersToBeSkippedSelector

-- | @- setCharactersToBeSkipped:@
setCharactersToBeSkipped :: (IsNSScanner nsScanner, IsNSCharacterSet value) => nsScanner -> value -> IO ()
setCharactersToBeSkipped nsScanner value =
  sendMessage nsScanner setCharactersToBeSkippedSelector (toNSCharacterSet value)

-- | @- caseSensitive@
caseSensitive :: IsNSScanner nsScanner => nsScanner -> IO Bool
caseSensitive nsScanner =
  sendMessage nsScanner caseSensitiveSelector

-- | @- setCaseSensitive:@
setCaseSensitive :: IsNSScanner nsScanner => nsScanner -> Bool -> IO ()
setCaseSensitive nsScanner value =
  sendMessage nsScanner setCaseSensitiveSelector value

-- | @- locale@
locale :: IsNSScanner nsScanner => nsScanner -> IO RawId
locale nsScanner =
  sendMessage nsScanner localeSelector

-- | @- setLocale:@
setLocale :: IsNSScanner nsScanner => nsScanner -> RawId -> IO ()
setLocale nsScanner value =
  sendMessage nsScanner setLocaleSelector value

-- | @- atEnd@
atEnd :: IsNSScanner nsScanner => nsScanner -> IO Bool
atEnd nsScanner =
  sendMessage nsScanner atEndSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSScanner)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @scanDecimal:@
scanDecimalSelector :: Selector '[RawId] Bool
scanDecimalSelector = mkSelector "scanDecimal:"

-- | @Selector@ for @scanInt:@
scanIntSelector :: Selector '[Ptr CInt] Bool
scanIntSelector = mkSelector "scanInt:"

-- | @Selector@ for @scanInteger:@
scanIntegerSelector :: Selector '[Ptr CLong] Bool
scanIntegerSelector = mkSelector "scanInteger:"

-- | @Selector@ for @scanLongLong:@
scanLongLongSelector :: Selector '[Ptr CLong] Bool
scanLongLongSelector = mkSelector "scanLongLong:"

-- | @Selector@ for @scanUnsignedLongLong:@
scanUnsignedLongLongSelector :: Selector '[Ptr CULong] Bool
scanUnsignedLongLongSelector = mkSelector "scanUnsignedLongLong:"

-- | @Selector@ for @scanFloat:@
scanFloatSelector :: Selector '[Ptr CFloat] Bool
scanFloatSelector = mkSelector "scanFloat:"

-- | @Selector@ for @scanDouble:@
scanDoubleSelector :: Selector '[Ptr CDouble] Bool
scanDoubleSelector = mkSelector "scanDouble:"

-- | @Selector@ for @scanHexInt:@
scanHexIntSelector :: Selector '[Ptr CUInt] Bool
scanHexIntSelector = mkSelector "scanHexInt:"

-- | @Selector@ for @scanHexLongLong:@
scanHexLongLongSelector :: Selector '[Ptr CULong] Bool
scanHexLongLongSelector = mkSelector "scanHexLongLong:"

-- | @Selector@ for @scanHexFloat:@
scanHexFloatSelector :: Selector '[Ptr CFloat] Bool
scanHexFloatSelector = mkSelector "scanHexFloat:"

-- | @Selector@ for @scanHexDouble:@
scanHexDoubleSelector :: Selector '[Ptr CDouble] Bool
scanHexDoubleSelector = mkSelector "scanHexDouble:"

-- | @Selector@ for @scanString:intoString:@
scanString_intoStringSelector :: Selector '[Id NSString, Id NSString] Bool
scanString_intoStringSelector = mkSelector "scanString:intoString:"

-- | @Selector@ for @scanCharactersFromSet:intoString:@
scanCharactersFromSet_intoStringSelector :: Selector '[Id NSCharacterSet, Id NSString] Bool
scanCharactersFromSet_intoStringSelector = mkSelector "scanCharactersFromSet:intoString:"

-- | @Selector@ for @scanUpToString:intoString:@
scanUpToString_intoStringSelector :: Selector '[Id NSString, Id NSString] Bool
scanUpToString_intoStringSelector = mkSelector "scanUpToString:intoString:"

-- | @Selector@ for @scanUpToCharactersFromSet:intoString:@
scanUpToCharactersFromSet_intoStringSelector :: Selector '[Id NSCharacterSet, Id NSString] Bool
scanUpToCharactersFromSet_intoStringSelector = mkSelector "scanUpToCharactersFromSet:intoString:"

-- | @Selector@ for @scannerWithString:@
scannerWithStringSelector :: Selector '[Id NSString] (Id NSScanner)
scannerWithStringSelector = mkSelector "scannerWithString:"

-- | @Selector@ for @localizedScannerWithString:@
localizedScannerWithStringSelector :: Selector '[Id NSString] RawId
localizedScannerWithStringSelector = mkSelector "localizedScannerWithString:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @scanLocation@
scanLocationSelector :: Selector '[] CULong
scanLocationSelector = mkSelector "scanLocation"

-- | @Selector@ for @setScanLocation:@
setScanLocationSelector :: Selector '[CULong] ()
setScanLocationSelector = mkSelector "setScanLocation:"

-- | @Selector@ for @charactersToBeSkipped@
charactersToBeSkippedSelector :: Selector '[] (Id NSCharacterSet)
charactersToBeSkippedSelector = mkSelector "charactersToBeSkipped"

-- | @Selector@ for @setCharactersToBeSkipped:@
setCharactersToBeSkippedSelector :: Selector '[Id NSCharacterSet] ()
setCharactersToBeSkippedSelector = mkSelector "setCharactersToBeSkipped:"

-- | @Selector@ for @caseSensitive@
caseSensitiveSelector :: Selector '[] Bool
caseSensitiveSelector = mkSelector "caseSensitive"

-- | @Selector@ for @setCaseSensitive:@
setCaseSensitiveSelector :: Selector '[Bool] ()
setCaseSensitiveSelector = mkSelector "setCaseSensitive:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] RawId
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[RawId] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @atEnd@
atEndSelector :: Selector '[] Bool
atEndSelector = mkSelector "atEnd"

