{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKTextureLoader@.
module ObjC.GLKit.GLKTextureLoader
  ( GLKTextureLoader
  , IsGLKTextureLoader(..)
  , textureWithContentsOfFile_options_error
  , textureWithContentsOfURL_options_error
  , textureWithName_scaleFactor_bundle_options_error
  , textureWithContentsOfData_options_error
  , textureWithCGImage_options_error
  , cubeMapWithContentsOfFiles_options_error
  , cubeMapWithContentsOfFile_options_error
  , cubeMapWithContentsOfURL_options_error
  , initWithShareContext
  , textureWithContentsOfFile_options_queue_completionHandler
  , textureWithContentsOfURL_options_queue_completionHandler
  , textureWithName_scaleFactor_bundle_options_queue_completionHandler
  , textureWithContentsOfData_options_queue_completionHandler
  , textureWithCGImage_options_queue_completionHandler
  , cubeMapWithContentsOfFiles_options_queue_completionHandler
  , cubeMapWithContentsOfFile_options_queue_completionHandler
  , cubeMapWithContentsOfURL_options_queue_completionHandler
  , cubeMapWithContentsOfFile_options_errorSelector
  , cubeMapWithContentsOfFile_options_queue_completionHandlerSelector
  , cubeMapWithContentsOfFiles_options_errorSelector
  , cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector
  , cubeMapWithContentsOfURL_options_errorSelector
  , cubeMapWithContentsOfURL_options_queue_completionHandlerSelector
  , initWithShareContextSelector
  , textureWithCGImage_options_errorSelector
  , textureWithCGImage_options_queue_completionHandlerSelector
  , textureWithContentsOfData_options_errorSelector
  , textureWithContentsOfData_options_queue_completionHandlerSelector
  , textureWithContentsOfFile_options_errorSelector
  , textureWithContentsOfFile_options_queue_completionHandlerSelector
  , textureWithContentsOfURL_options_errorSelector
  , textureWithContentsOfURL_options_queue_completionHandlerSelector
  , textureWithName_scaleFactor_bundle_options_errorSelector
  , textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ textureWithContentsOfFile:options:error:@
textureWithContentsOfFile_options_error :: (IsNSString path, IsNSDictionary options, IsNSError outError) => path -> options -> outError -> IO (Id GLKTextureInfo)
textureWithContentsOfFile_options_error path options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' textureWithContentsOfFile_options_errorSelector (toNSString path) (toNSDictionary options) (toNSError outError)

-- | @+ textureWithContentsOfURL:options:error:@
textureWithContentsOfURL_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError outError) => url -> options -> outError -> IO (Id GLKTextureInfo)
textureWithContentsOfURL_options_error url options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' textureWithContentsOfURL_options_errorSelector (toNSURL url) (toNSDictionary options) (toNSError outError)

-- | @+ textureWithName:scaleFactor:bundle:options:error:@
textureWithName_scaleFactor_bundle_options_error :: (IsNSString name, IsNSBundle bundle, IsNSDictionary options, IsNSError outError) => name -> CDouble -> bundle -> options -> outError -> IO (Id GLKTextureInfo)
textureWithName_scaleFactor_bundle_options_error name scaleFactor bundle options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' textureWithName_scaleFactor_bundle_options_errorSelector (toNSString name) scaleFactor (toNSBundle bundle) (toNSDictionary options) (toNSError outError)

-- | @+ textureWithContentsOfData:options:error:@
textureWithContentsOfData_options_error :: (IsNSData data_, IsNSDictionary options, IsNSError outError) => data_ -> options -> outError -> IO (Id GLKTextureInfo)
textureWithContentsOfData_options_error data_ options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' textureWithContentsOfData_options_errorSelector (toNSData data_) (toNSDictionary options) (toNSError outError)

-- | @+ textureWithCGImage:options:error:@
textureWithCGImage_options_error :: (IsNSDictionary options, IsNSError outError) => Ptr () -> options -> outError -> IO (Id GLKTextureInfo)
textureWithCGImage_options_error cgImage options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' textureWithCGImage_options_errorSelector cgImage (toNSDictionary options) (toNSError outError)

-- | @+ cubeMapWithContentsOfFiles:options:error:@
cubeMapWithContentsOfFiles_options_error :: (IsNSArray paths, IsNSDictionary options, IsNSError outError) => paths -> options -> outError -> IO (Id GLKTextureInfo)
cubeMapWithContentsOfFiles_options_error paths options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' cubeMapWithContentsOfFiles_options_errorSelector (toNSArray paths) (toNSDictionary options) (toNSError outError)

-- | @+ cubeMapWithContentsOfFile:options:error:@
cubeMapWithContentsOfFile_options_error :: (IsNSString path, IsNSDictionary options, IsNSError outError) => path -> options -> outError -> IO (Id GLKTextureInfo)
cubeMapWithContentsOfFile_options_error path options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' cubeMapWithContentsOfFile_options_errorSelector (toNSString path) (toNSDictionary options) (toNSError outError)

-- | @+ cubeMapWithContentsOfURL:options:error:@
cubeMapWithContentsOfURL_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError outError) => url -> options -> outError -> IO (Id GLKTextureInfo)
cubeMapWithContentsOfURL_options_error url options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    sendClassMessage cls' cubeMapWithContentsOfURL_options_errorSelector (toNSURL url) (toNSDictionary options) (toNSError outError)

-- | @- initWithShareContext:@
initWithShareContext :: (IsGLKTextureLoader glkTextureLoader, IsNSOpenGLContext context) => glkTextureLoader -> context -> IO (Id GLKTextureLoader)
initWithShareContext glkTextureLoader context =
  sendOwnedMessage glkTextureLoader initWithShareContextSelector (toNSOpenGLContext context)

-- | @- textureWithContentsOfFile:options:queue:completionHandler:@
textureWithContentsOfFile_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSString path, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> path -> options -> queue -> Ptr () -> IO ()
textureWithContentsOfFile_options_queue_completionHandler glkTextureLoader path options queue block =
  sendMessage glkTextureLoader textureWithContentsOfFile_options_queue_completionHandlerSelector (toNSString path) (toNSDictionary options) (toNSObject queue) block

-- | @- textureWithContentsOfURL:options:queue:completionHandler:@
textureWithContentsOfURL_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSURL url, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> url -> options -> queue -> Ptr () -> IO ()
textureWithContentsOfURL_options_queue_completionHandler glkTextureLoader url options queue block =
  sendMessage glkTextureLoader textureWithContentsOfURL_options_queue_completionHandlerSelector (toNSURL url) (toNSDictionary options) (toNSObject queue) block

-- | @- textureWithName:scaleFactor:bundle:options:queue:completionHandler:@
textureWithName_scaleFactor_bundle_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSString name, IsNSBundle bundle, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> name -> CDouble -> bundle -> options -> queue -> Ptr () -> IO ()
textureWithName_scaleFactor_bundle_options_queue_completionHandler glkTextureLoader name scaleFactor bundle options queue block =
  sendMessage glkTextureLoader textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector (toNSString name) scaleFactor (toNSBundle bundle) (toNSDictionary options) (toNSObject queue) block

-- | @- textureWithContentsOfData:options:queue:completionHandler:@
textureWithContentsOfData_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSData data_, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> data_ -> options -> queue -> Ptr () -> IO ()
textureWithContentsOfData_options_queue_completionHandler glkTextureLoader data_ options queue block =
  sendMessage glkTextureLoader textureWithContentsOfData_options_queue_completionHandlerSelector (toNSData data_) (toNSDictionary options) (toNSObject queue) block

-- | @- textureWithCGImage:options:queue:completionHandler:@
textureWithCGImage_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> Ptr () -> options -> queue -> Ptr () -> IO ()
textureWithCGImage_options_queue_completionHandler glkTextureLoader cgImage options queue block =
  sendMessage glkTextureLoader textureWithCGImage_options_queue_completionHandlerSelector cgImage (toNSDictionary options) (toNSObject queue) block

-- | @- cubeMapWithContentsOfFiles:options:queue:completionHandler:@
cubeMapWithContentsOfFiles_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSArray paths, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> paths -> options -> queue -> Ptr () -> IO ()
cubeMapWithContentsOfFiles_options_queue_completionHandler glkTextureLoader paths options queue block =
  sendMessage glkTextureLoader cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector (toNSArray paths) (toNSDictionary options) (toNSObject queue) block

-- | @- cubeMapWithContentsOfFile:options:queue:completionHandler:@
cubeMapWithContentsOfFile_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSString path, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> path -> options -> queue -> Ptr () -> IO ()
cubeMapWithContentsOfFile_options_queue_completionHandler glkTextureLoader path options queue block =
  sendMessage glkTextureLoader cubeMapWithContentsOfFile_options_queue_completionHandlerSelector (toNSString path) (toNSDictionary options) (toNSObject queue) block

-- | @- cubeMapWithContentsOfURL:options:queue:completionHandler:@
cubeMapWithContentsOfURL_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSURL url, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> url -> options -> queue -> Ptr () -> IO ()
cubeMapWithContentsOfURL_options_queue_completionHandler glkTextureLoader url options queue block =
  sendMessage glkTextureLoader cubeMapWithContentsOfURL_options_queue_completionHandlerSelector (toNSURL url) (toNSDictionary options) (toNSObject queue) block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textureWithContentsOfFile:options:error:@
textureWithContentsOfFile_options_errorSelector :: Selector '[Id NSString, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
textureWithContentsOfFile_options_errorSelector = mkSelector "textureWithContentsOfFile:options:error:"

-- | @Selector@ for @textureWithContentsOfURL:options:error:@
textureWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
textureWithContentsOfURL_options_errorSelector = mkSelector "textureWithContentsOfURL:options:error:"

-- | @Selector@ for @textureWithName:scaleFactor:bundle:options:error:@
textureWithName_scaleFactor_bundle_options_errorSelector :: Selector '[Id NSString, CDouble, Id NSBundle, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
textureWithName_scaleFactor_bundle_options_errorSelector = mkSelector "textureWithName:scaleFactor:bundle:options:error:"

-- | @Selector@ for @textureWithContentsOfData:options:error:@
textureWithContentsOfData_options_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
textureWithContentsOfData_options_errorSelector = mkSelector "textureWithContentsOfData:options:error:"

-- | @Selector@ for @textureWithCGImage:options:error:@
textureWithCGImage_options_errorSelector :: Selector '[Ptr (), Id NSDictionary, Id NSError] (Id GLKTextureInfo)
textureWithCGImage_options_errorSelector = mkSelector "textureWithCGImage:options:error:"

-- | @Selector@ for @cubeMapWithContentsOfFiles:options:error:@
cubeMapWithContentsOfFiles_options_errorSelector :: Selector '[Id NSArray, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
cubeMapWithContentsOfFiles_options_errorSelector = mkSelector "cubeMapWithContentsOfFiles:options:error:"

-- | @Selector@ for @cubeMapWithContentsOfFile:options:error:@
cubeMapWithContentsOfFile_options_errorSelector :: Selector '[Id NSString, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
cubeMapWithContentsOfFile_options_errorSelector = mkSelector "cubeMapWithContentsOfFile:options:error:"

-- | @Selector@ for @cubeMapWithContentsOfURL:options:error:@
cubeMapWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] (Id GLKTextureInfo)
cubeMapWithContentsOfURL_options_errorSelector = mkSelector "cubeMapWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithShareContext:@
initWithShareContextSelector :: Selector '[Id NSOpenGLContext] (Id GLKTextureLoader)
initWithShareContextSelector = mkSelector "initWithShareContext:"

-- | @Selector@ for @textureWithContentsOfFile:options:queue:completionHandler:@
textureWithContentsOfFile_options_queue_completionHandlerSelector :: Selector '[Id NSString, Id NSDictionary, Id NSObject, Ptr ()] ()
textureWithContentsOfFile_options_queue_completionHandlerSelector = mkSelector "textureWithContentsOfFile:options:queue:completionHandler:"

-- | @Selector@ for @textureWithContentsOfURL:options:queue:completionHandler:@
textureWithContentsOfURL_options_queue_completionHandlerSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSObject, Ptr ()] ()
textureWithContentsOfURL_options_queue_completionHandlerSelector = mkSelector "textureWithContentsOfURL:options:queue:completionHandler:"

-- | @Selector@ for @textureWithName:scaleFactor:bundle:options:queue:completionHandler:@
textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector :: Selector '[Id NSString, CDouble, Id NSBundle, Id NSDictionary, Id NSObject, Ptr ()] ()
textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector = mkSelector "textureWithName:scaleFactor:bundle:options:queue:completionHandler:"

-- | @Selector@ for @textureWithContentsOfData:options:queue:completionHandler:@
textureWithContentsOfData_options_queue_completionHandlerSelector :: Selector '[Id NSData, Id NSDictionary, Id NSObject, Ptr ()] ()
textureWithContentsOfData_options_queue_completionHandlerSelector = mkSelector "textureWithContentsOfData:options:queue:completionHandler:"

-- | @Selector@ for @textureWithCGImage:options:queue:completionHandler:@
textureWithCGImage_options_queue_completionHandlerSelector :: Selector '[Ptr (), Id NSDictionary, Id NSObject, Ptr ()] ()
textureWithCGImage_options_queue_completionHandlerSelector = mkSelector "textureWithCGImage:options:queue:completionHandler:"

-- | @Selector@ for @cubeMapWithContentsOfFiles:options:queue:completionHandler:@
cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector :: Selector '[Id NSArray, Id NSDictionary, Id NSObject, Ptr ()] ()
cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector = mkSelector "cubeMapWithContentsOfFiles:options:queue:completionHandler:"

-- | @Selector@ for @cubeMapWithContentsOfFile:options:queue:completionHandler:@
cubeMapWithContentsOfFile_options_queue_completionHandlerSelector :: Selector '[Id NSString, Id NSDictionary, Id NSObject, Ptr ()] ()
cubeMapWithContentsOfFile_options_queue_completionHandlerSelector = mkSelector "cubeMapWithContentsOfFile:options:queue:completionHandler:"

-- | @Selector@ for @cubeMapWithContentsOfURL:options:queue:completionHandler:@
cubeMapWithContentsOfURL_options_queue_completionHandlerSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSObject, Ptr ()] ()
cubeMapWithContentsOfURL_options_queue_completionHandlerSelector = mkSelector "cubeMapWithContentsOfURL:options:queue:completionHandler:"

