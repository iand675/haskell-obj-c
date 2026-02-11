{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.UniformTypeIdentifiers.NSString
  ( NSString
  , IsNSString(..)
  , stringByAppendingPathComponent_conformingToType
  , stringByAppendingPathExtensionForType
  , stringByAppendingPathComponent_conformingToTypeSelector
  , stringByAppendingPathExtensionForTypeSelector


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

import ObjC.UniformTypeIdentifiers.Internal.Classes
import ObjC.Foundation.Internal.Classes
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | Generate a path component based on a partial filename and a file		type, then append it to a copy of the receiver.
--
-- @partialName@ — The partial filename that should be expanded upon,		e.g. @"readme".@
--
-- @contentType@ — The type the resulting file should conform to, e.g.		@UTTypePlainText.@
--
-- Returns: A complete file path. Using the argument examples above, this method		would return a string with a last path component of @"readme.txt".@
--
-- Use this method when you have partial input from a user or other source and	need to produce a complete filename suitable for that input. For example, if	you are downloading a file from the Internet and know its MIME type, you can	use this method to ensure the correct filename extension is applied to the	URL where you save the file.
--
-- If partialName already has a path extension, and that path extension is	valid for file system objects of type contentType, no additional	extension is appended to the path component before constructing the string.	For example, if the inputs are @"puppy.jpg"@ and @UTTypeImage@	respectively, then the resulting string will have a last path component of	@"puppy.jpg"@ . On the other hand, if the inputs are @"puppy.jpg"@ and	@UTTypePlainText@ respectively, the resulting string will have a last path	component of @"puppy.jpg.txt"@ . If you want to ensure any existing path	extension is replaced, you can use the @stringByDeletingPathExtension@	property first.
--
-- If the path component could not be appended, this method returns a copy of	@self@ .
--
-- ObjC selector: @- stringByAppendingPathComponent:conformingToType:@
stringByAppendingPathComponent_conformingToType :: (IsNSString nsString, IsNSString partialName, IsUTType contentType) => nsString -> partialName -> contentType -> IO (Id NSString)
stringByAppendingPathComponent_conformingToType nsString  partialName contentType =
withObjCPtr partialName $ \raw_partialName ->
  withObjCPtr contentType $ \raw_contentType ->
      sendMsg nsString (mkSelector "stringByAppendingPathComponent:conformingToType:") (retPtr retVoid) [argPtr (castPtr raw_partialName :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr

-- | Generate a string based on a partial filename or path and a		file type.
--
-- @contentType@ — The type the resulting file should conform to, e.g.		@UTTypePlainText.@
--
-- Returns: A complete file path. Using the argument example above and assuming		the receiver equals @"readme"@ , this method would return		@"readme.txt".@
--
-- Use this method when you have partial input from a user or other source and	need to produce a complete filename suitable for that input. For example, if	you are downloading a file from the Internet and know its MIME type, you can	use this method to ensure the correct filename extension is applied to the	URL where you save the file.
--
-- If the receiver already has a path extension, and that path extension is	valid for file system objects of type contentType, no additional	extension is appended to the receiver before constructing the result.	For example, if the receiver equals @"puppy.jpg"@ and contentType equals	@UTTypeImage@ , then the resulting string will equal @"puppy.jpg"@ . On	the other hand, if the inputs are @"puppy.jpg"@ and @UTTypePlainText@	respectively, the resulting string will equal @"puppy.jpg.txt"@ . If you	want to ensure any existing path extension is replaced, you can use the	@stringByDeletingPathExtension@ property first.
--
-- If the extension could not be appended, this method returns a copy of	@self@ .
--
-- ObjC selector: @- stringByAppendingPathExtensionForType:@
stringByAppendingPathExtensionForType :: (IsNSString nsString, IsUTType contentType) => nsString -> contentType -> IO (Id NSString)
stringByAppendingPathExtensionForType nsString  contentType =
withObjCPtr contentType $ \raw_contentType ->
    sendMsg nsString (mkSelector "stringByAppendingPathExtensionForType:") (retPtr retVoid) [argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringByAppendingPathComponent:conformingToType:@
stringByAppendingPathComponent_conformingToTypeSelector :: Selector
stringByAppendingPathComponent_conformingToTypeSelector = mkSelector "stringByAppendingPathComponent:conformingToType:"

-- | @Selector@ for @stringByAppendingPathExtensionForType:@
stringByAppendingPathExtensionForTypeSelector :: Selector
stringByAppendingPathExtensionForTypeSelector = mkSelector "stringByAppendingPathExtensionForType:"

