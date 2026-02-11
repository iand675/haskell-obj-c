{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURL@.
module ObjC.UniformTypeIdentifiers.NSURL
  ( NSURL
  , IsNSURL(..)
  , urlByAppendingPathComponent_conformingToType
  , urlByAppendingPathExtensionForType
  , urlByAppendingPathComponent_conformingToTypeSelector
  , urlByAppendingPathExtensionForTypeSelector


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

-- | Generate a path component based on a partial filename and a file		type, then append it to a copy of the receiver.
--
-- @partialName@ — The partial filename that should be expanded upon,		e.g. @"readme".@
--
-- @contentType@ — The type the resulting file should conform to, e.g.		@UTTypePlainText.@
--
-- Returns: A complete URL. Using the argument examples above, this method would		return a URL with a last path component of @"readme.txt".@
--
-- Use this method when you have partial input from a user or other source and	need to produce a complete filename suitable for that input. For example, if	you are downloading a file from the Internet and know its MIME type, you can	use this method to ensure the correct filename extension is applied to the	URL where you save the file.
--
-- If partialName already has a path extension, and that path extension is	valid for file system objects of type contentType, no additional	extension is appended to the path component before constructing the URL. For	example, if the inputs are @"puppy.jpg"@ and @UTTypeImage@ respectively,	then the resulting URL will have a last path component of @"puppy.jpg"@ .	On the other hand, if the inputs are @"puppy.jpg"@ and @UTTypePlainText@	respectively, the resulting URL will have a last path component of	@"puppy.jpg.txt"@ . If you want to ensure any existing path extension is	replaced, you can use the @URLByDeletingPathExtension@ property first.
--
-- If the path component could not be appended, this method returns a copy of	@self@ .
--
-- Note: The resulting URL has a directory path if @contentType@ conforms to		@UTTypeDirectory@ .
--
-- ObjC selector: @- URLByAppendingPathComponent:conformingToType:@
urlByAppendingPathComponent_conformingToType :: (IsNSURL nsurl, IsNSString partialName, IsUTType contentType) => nsurl -> partialName -> contentType -> IO (Id NSURL)
urlByAppendingPathComponent_conformingToType nsurl  partialName contentType =
withObjCPtr partialName $ \raw_partialName ->
  withObjCPtr contentType $ \raw_contentType ->
      sendMsg nsurl (mkSelector "URLByAppendingPathComponent:conformingToType:") (retPtr retVoid) [argPtr (castPtr raw_partialName :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr

-- | Generate a path component based on the last path component of the		receiver and a file type, then append it to a copy of the receiver.
--
-- @contentType@ — The type the resulting file should conform to, e.g.		@UTTypePlainText.@
--
-- Returns: A complete URL. Using the argument example above and assuming		the receiver equals @"file:///readme"@ , this method would return		@"file:///readme.txt".@
--
-- Use this method when you have partial input from a user or other source and	need to produce a complete filename suitable for that input. For example, if	you are downloading a file from the Internet and know its MIME type, you can	use this method to ensure the correct filename extension is applied to the	URL where you save the file.
--
-- If the receiver already has a path extension, and that path extension is	valid for file system objects of type contentType, no additional	extension is appended to the path component before constructing the URL.	For example, if the receiver's last path component equals @"puppy.jpg"@ and	contentType equals @UTTypeImage@ , then the resulting URL will have a	last path component of @"puppy.jpg"@ . On the other hand, if the inputs are	@"puppy.jpg"@ and @UTTypePlainText@ respectively, the resulting URL will	have a last path component of @"puppy.jpg.txt"@ . If you want to ensure any	existing path extension is replaced, you can use the	@URLByDeletingPathExtension@ property first.
--
-- If the extension could not be appended, this method returns a copy of	@self@ .
--
-- Note: The resulting URL has a directory path if @contentType@ conforms to		@UTTypeDirectory@ .
--
-- ObjC selector: @- URLByAppendingPathExtensionForType:@
urlByAppendingPathExtensionForType :: (IsNSURL nsurl, IsUTType contentType) => nsurl -> contentType -> IO (Id NSURL)
urlByAppendingPathExtensionForType nsurl  contentType =
withObjCPtr contentType $ \raw_contentType ->
    sendMsg nsurl (mkSelector "URLByAppendingPathExtensionForType:") (retPtr retVoid) [argPtr (castPtr raw_contentType :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URLByAppendingPathComponent:conformingToType:@
urlByAppendingPathComponent_conformingToTypeSelector :: Selector
urlByAppendingPathComponent_conformingToTypeSelector = mkSelector "URLByAppendingPathComponent:conformingToType:"

-- | @Selector@ for @URLByAppendingPathExtensionForType:@
urlByAppendingPathExtensionForTypeSelector :: Selector
urlByAppendingPathExtensionForTypeSelector = mkSelector "URLByAppendingPathExtensionForType:"

