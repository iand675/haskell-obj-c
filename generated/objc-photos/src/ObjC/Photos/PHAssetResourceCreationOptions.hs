{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetResourceCreationOptions@.
module ObjC.Photos.PHAssetResourceCreationOptions
  ( PHAssetResourceCreationOptions
  , IsPHAssetResourceCreationOptions(..)
  , originalFilename
  , setOriginalFilename
  , contentType
  , setContentType
  , uniformTypeIdentifier
  , setUniformTypeIdentifier
  , shouldMoveFile
  , setShouldMoveFile
  , originalFilenameSelector
  , setOriginalFilenameSelector
  , contentTypeSelector
  , setContentTypeSelector
  , uniformTypeIdentifierSelector
  , setUniformTypeIdentifierSelector
  , shouldMoveFileSelector
  , setShouldMoveFileSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- originalFilename@
originalFilename :: IsPHAssetResourceCreationOptions phAssetResourceCreationOptions => phAssetResourceCreationOptions -> IO (Id NSString)
originalFilename phAssetResourceCreationOptions  =
  sendMsg phAssetResourceCreationOptions (mkSelector "originalFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginalFilename:@
setOriginalFilename :: (IsPHAssetResourceCreationOptions phAssetResourceCreationOptions, IsNSString value) => phAssetResourceCreationOptions -> value -> IO ()
setOriginalFilename phAssetResourceCreationOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg phAssetResourceCreationOptions (mkSelector "setOriginalFilename:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The type of data being provided for this asset resource. If not specified, one will be inferred from the PHAssetResourceType or file URL extension (if provided).
--
-- ObjC selector: @- contentType@
contentType :: IsPHAssetResourceCreationOptions phAssetResourceCreationOptions => phAssetResourceCreationOptions -> IO (Id UTType)
contentType phAssetResourceCreationOptions  =
  sendMsg phAssetResourceCreationOptions (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of data being provided for this asset resource. If not specified, one will be inferred from the PHAssetResourceType or file URL extension (if provided).
--
-- ObjC selector: @- setContentType:@
setContentType :: (IsPHAssetResourceCreationOptions phAssetResourceCreationOptions, IsUTType value) => phAssetResourceCreationOptions -> value -> IO ()
setContentType phAssetResourceCreationOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg phAssetResourceCreationOptions (mkSelector "setContentType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- uniformTypeIdentifier@
uniformTypeIdentifier :: IsPHAssetResourceCreationOptions phAssetResourceCreationOptions => phAssetResourceCreationOptions -> IO (Id NSString)
uniformTypeIdentifier phAssetResourceCreationOptions  =
  sendMsg phAssetResourceCreationOptions (mkSelector "uniformTypeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniformTypeIdentifier:@
setUniformTypeIdentifier :: (IsPHAssetResourceCreationOptions phAssetResourceCreationOptions, IsNSString value) => phAssetResourceCreationOptions -> value -> IO ()
setUniformTypeIdentifier phAssetResourceCreationOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg phAssetResourceCreationOptions (mkSelector "setUniformTypeIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldMoveFile@
shouldMoveFile :: IsPHAssetResourceCreationOptions phAssetResourceCreationOptions => phAssetResourceCreationOptions -> IO Bool
shouldMoveFile phAssetResourceCreationOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAssetResourceCreationOptions (mkSelector "shouldMoveFile") retCULong []

-- | @- setShouldMoveFile:@
setShouldMoveFile :: IsPHAssetResourceCreationOptions phAssetResourceCreationOptions => phAssetResourceCreationOptions -> Bool -> IO ()
setShouldMoveFile phAssetResourceCreationOptions  value =
  sendMsg phAssetResourceCreationOptions (mkSelector "setShouldMoveFile:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @originalFilename@
originalFilenameSelector :: Selector
originalFilenameSelector = mkSelector "originalFilename"

-- | @Selector@ for @setOriginalFilename:@
setOriginalFilenameSelector :: Selector
setOriginalFilenameSelector = mkSelector "setOriginalFilename:"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @setContentType:@
setContentTypeSelector :: Selector
setContentTypeSelector = mkSelector "setContentType:"

-- | @Selector@ for @uniformTypeIdentifier@
uniformTypeIdentifierSelector :: Selector
uniformTypeIdentifierSelector = mkSelector "uniformTypeIdentifier"

-- | @Selector@ for @setUniformTypeIdentifier:@
setUniformTypeIdentifierSelector :: Selector
setUniformTypeIdentifierSelector = mkSelector "setUniformTypeIdentifier:"

-- | @Selector@ for @shouldMoveFile@
shouldMoveFileSelector :: Selector
shouldMoveFileSelector = mkSelector "shouldMoveFile"

-- | @Selector@ for @setShouldMoveFile:@
setShouldMoveFileSelector :: Selector
setShouldMoveFileSelector = mkSelector "setShouldMoveFile:"

