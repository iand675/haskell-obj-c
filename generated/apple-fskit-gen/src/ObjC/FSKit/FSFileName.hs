{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The name of a file, expressed as a data buffer.
--
-- @FSFileName@ is the class that carries filenames from the kernel to @FSModule@ instances, and carries names back to the kernel as part of directory enumeration.
--
-- A filename is usually a valid UTF-8 sequence, but can be an arbitrary byte sequence that doesn't conform to that format. As a result, the ``data`` property always contains a value, but the ``string`` property may be empty. An @FSModule@ can receive an @FSFileName@ that isn't valid UTF-8 in two cases: 1. A program passes erroneous data to a system call. The @FSModule@ treats this situation as an error. 2. An @FSModule@ lacks the character encoding used for a file name. This situation occurs because some file system formats consider a filename to be an arbitrary "bag of bytes," and leave character encoding up to the operating system. Without encoding information, the @FSModule@ can only pass back the names it finds on disk. In this case, the behavior of upper layers such as <doc://com.apple.documentation/documentation/Foundation/NSFileManager> is unspecified. However, the @FSModule@ must support looking up such names and using them as the source name of rename operations. The @FSModule@ must also be able to support filenames that are derivatives of filenames returned from directory enumeration. Derivative filenames include Apple Double filenames (@"._Name"@), and editor backup filenames.
--
-- > Important: Don't subclass this class.
--
-- Generated bindings for @FSFileName@.
module ObjC.FSKit.FSFileName
  ( FSFileName
  , IsFSFileName(..)
  , init_
  , initWithCString
  , initWithBytes_length
  , initWithData
  , initWithString
  , nameWithCString
  , nameWithBytes_length
  , nameWithData
  , nameWithString
  , data_
  , string
  , debugDescription
  , dataSelector
  , debugDescriptionSelector
  , initSelector
  , initWithBytes_lengthSelector
  , initWithCStringSelector
  , initWithDataSelector
  , initWithStringSelector
  , nameWithBytes_lengthSelector
  , nameWithCStringSelector
  , nameWithDataSelector
  , nameWithStringSelector
  , stringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSFileName fsFileName => fsFileName -> IO (Id FSFileName)
init_ fsFileName =
  sendOwnedMessage fsFileName initSelector

-- | Initializes a filename from a null-terminated character sequence.
--
-- > Note: This initializer is unavailable in Swift. Use ``initWithData:`` or ``initWithString:`` instead.
--
-- - Parameter name: A pointer to a C string.
--
-- ObjC selector: @- initWithCString:@
initWithCString :: IsFSFileName fsFileName => fsFileName -> Const (Ptr CChar) -> IO (Id FSFileName)
initWithCString fsFileName name =
  sendOwnedMessage fsFileName initWithCStringSelector name

-- | Initializes a file name by copying a character sequence from a byte array.
--
-- > Note: This initializer is unavailable in Swift. Use ``initWithData:`` or ``initWithString:`` instead.
--
-- - Parameters:  - bytes: A pointer to the character data to copy, up to a maximum of @length@. The sequence terminates if a @NUL@ character exists prior to @length@.  - length: The size of the @bytes@ array.
--
-- ObjC selector: @- initWithBytes:length:@
initWithBytes_length :: IsFSFileName fsFileName => fsFileName -> Const (Ptr CChar) -> CULong -> IO (Id FSFileName)
initWithBytes_length fsFileName bytes length_ =
  sendOwnedMessage fsFileName initWithBytes_lengthSelector bytes length_

-- | Creates a filename by copying a character sequence data object.
--
-- This initializer copies up to @name.length@ characters of the sequence pointed to by @bytes@.
--
-- - Parameter name: The data object containing the character sequence to use for the filename. The sequence terminates if a @NUL@ character exists prior to @name.length@.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsFSFileName fsFileName, IsNSData name) => fsFileName -> name -> IO (Id FSFileName)
initWithData fsFileName name =
  sendOwnedMessage fsFileName initWithDataSelector (toNSData name)

-- | Creates a filename by copying a character sequence from a string instance.
--
-- This initializer copies the UTF-8 representation of the characters in @string@. If @string@ contains a @NUL@ character, the sequence terminates.
--
-- - Parameter name: The string containing the character sequence to use for the filename.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsFSFileName fsFileName, IsNSString name) => fsFileName -> name -> IO (Id FSFileName)
initWithString fsFileName name =
  sendOwnedMessage fsFileName initWithStringSelector (toNSString name)

-- | Creates a filename from a null-terminated character sequence.
--
-- - Parameter name: A pointer to a C string.
--
-- ObjC selector: @+ nameWithCString:@
nameWithCString :: Const (Ptr CChar) -> IO (Id FSFileName)
nameWithCString name =
  do
    cls' <- getRequiredClass "FSFileName"
    sendClassMessage cls' nameWithCStringSelector name

-- | Creates a filename by copying a character sequence from a byte array.
--
-- - Parameters:  - bytes: A pointer to the character data to copy, up to a maximum of @length@. The sequence terminates if a @NUL@ character exists prior to @length@.  - length: The size of the @bytes@ array.
--
-- ObjC selector: @+ nameWithBytes:length:@
nameWithBytes_length :: Const (Ptr CChar) -> CULong -> IO (Id FSFileName)
nameWithBytes_length bytes length_ =
  do
    cls' <- getRequiredClass "FSFileName"
    sendClassMessage cls' nameWithBytes_lengthSelector bytes length_

-- | Creates a filename by copying a character sequence data object.
--
-- This initializer copies up to @name.length@ characters of the sequence pointed to by @bytes@.
--
-- - Parameter name: The data object containing the character sequence to use for the filename. The sequence terminates if a @NUL@ character exists prior to @name.length@.
--
-- ObjC selector: @+ nameWithData:@
nameWithData :: IsNSData name => name -> IO (Id FSFileName)
nameWithData name =
  do
    cls' <- getRequiredClass "FSFileName"
    sendClassMessage cls' nameWithDataSelector (toNSData name)

-- | Creates a filename by copying a character sequence from a string instance.
--
-- This initializer copies the UTF-8 representation of the characters in @string@. If @string@ contains a @NUL@ character, the sequence terminates.
--
-- - Parameter name: The string containing the character sequence to use for the filename.
--
-- ObjC selector: @+ nameWithString:@
nameWithString :: IsNSString name => name -> IO (Id FSFileName)
nameWithString name =
  do
    cls' <- getRequiredClass "FSFileName"
    sendClassMessage cls' nameWithStringSelector (toNSString name)

-- | The byte sequence of the filename, as a data object.
--
-- This property always provides a value.
--
-- ObjC selector: @- data@
data_ :: IsFSFileName fsFileName => fsFileName -> IO (Id NSData)
data_ fsFileName =
  sendMessage fsFileName dataSelector

-- | The filename, represented as a Unicode string.
--
-- If the value of the filename's ``FSFileName/data`` is not a valid UTF-8 byte sequence, this property is empty.
--
-- ObjC selector: @- string@
string :: IsFSFileName fsFileName => fsFileName -> IO (Id NSString)
string fsFileName =
  sendMessage fsFileName stringSelector

-- | The filename, represented as a potentially lossy conversion to a string.
--
-- The exact details of the string conversion may change in the future.
--
-- ObjC selector: @- debugDescription@
debugDescription :: IsFSFileName fsFileName => fsFileName -> IO (Id NSString)
debugDescription fsFileName =
  sendMessage fsFileName debugDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSFileName)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCString:@
initWithCStringSelector :: Selector '[Const (Ptr CChar)] (Id FSFileName)
initWithCStringSelector = mkSelector "initWithCString:"

-- | @Selector@ for @initWithBytes:length:@
initWithBytes_lengthSelector :: Selector '[Const (Ptr CChar), CULong] (Id FSFileName)
initWithBytes_lengthSelector = mkSelector "initWithBytes:length:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id FSFileName)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id FSFileName)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @nameWithCString:@
nameWithCStringSelector :: Selector '[Const (Ptr CChar)] (Id FSFileName)
nameWithCStringSelector = mkSelector "nameWithCString:"

-- | @Selector@ for @nameWithBytes:length:@
nameWithBytes_lengthSelector :: Selector '[Const (Ptr CChar), CULong] (Id FSFileName)
nameWithBytes_lengthSelector = mkSelector "nameWithBytes:length:"

-- | @Selector@ for @nameWithData:@
nameWithDataSelector :: Selector '[Id NSData] (Id FSFileName)
nameWithDataSelector = mkSelector "nameWithData:"

-- | @Selector@ for @nameWithString:@
nameWithStringSelector :: Selector '[Id NSString] (Id FSFileName)
nameWithStringSelector = mkSelector "nameWithString:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @debugDescription@
debugDescriptionSelector :: Selector '[] (Id NSString)
debugDescriptionSelector = mkSelector "debugDescription"

