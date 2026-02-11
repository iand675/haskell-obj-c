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
  , textureWithContentsOfFile_options_errorSelector
  , textureWithContentsOfURL_options_errorSelector
  , textureWithName_scaleFactor_bundle_options_errorSelector
  , textureWithContentsOfData_options_errorSelector
  , textureWithCGImage_options_errorSelector
  , cubeMapWithContentsOfFiles_options_errorSelector
  , cubeMapWithContentsOfFile_options_errorSelector
  , cubeMapWithContentsOfURL_options_errorSelector
  , initWithShareContextSelector
  , textureWithContentsOfFile_options_queue_completionHandlerSelector
  , textureWithContentsOfURL_options_queue_completionHandlerSelector
  , textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector
  , textureWithContentsOfData_options_queue_completionHandlerSelector
  , textureWithCGImage_options_queue_completionHandlerSelector
  , cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector
  , cubeMapWithContentsOfFile_options_queue_completionHandlerSelector
  , cubeMapWithContentsOfURL_options_queue_completionHandlerSelector


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

import ObjC.GLKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ textureWithContentsOfFile:options:error:@
textureWithContentsOfFile_options_error :: (IsNSString path, IsNSDictionary options, IsNSError outError) => path -> options -> outError -> IO (Id GLKTextureInfo)
textureWithContentsOfFile_options_error path options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr path $ \raw_path ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "textureWithContentsOfFile:options:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureWithContentsOfURL:options:error:@
textureWithContentsOfURL_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError outError) => url -> options -> outError -> IO (Id GLKTextureInfo)
textureWithContentsOfURL_options_error url options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "textureWithContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureWithName:scaleFactor:bundle:options:error:@
textureWithName_scaleFactor_bundle_options_error :: (IsNSString name, IsNSBundle bundle, IsNSDictionary options, IsNSError outError) => name -> CDouble -> bundle -> options -> outError -> IO (Id GLKTextureInfo)
textureWithName_scaleFactor_bundle_options_error name scaleFactor bundle options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr name $ \raw_name ->
      withObjCPtr bundle $ \raw_bundle ->
        withObjCPtr options $ \raw_options ->
          withObjCPtr outError $ \raw_outError ->
            sendClassMsg cls' (mkSelector "textureWithName:scaleFactor:bundle:options:error:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCDouble (fromIntegral scaleFactor), argPtr (castPtr raw_bundle :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureWithContentsOfData:options:error:@
textureWithContentsOfData_options_error :: (IsNSData data_, IsNSDictionary options, IsNSError outError) => data_ -> options -> outError -> IO (Id GLKTextureInfo)
textureWithContentsOfData_options_error data_ options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "textureWithContentsOfData:options:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ textureWithCGImage:options:error:@
textureWithCGImage_options_error :: (IsNSDictionary options, IsNSError outError) => Ptr () -> options -> outError -> IO (Id GLKTextureInfo)
textureWithCGImage_options_error cgImage options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr options $ \raw_options ->
      withObjCPtr outError $ \raw_outError ->
        sendClassMsg cls' (mkSelector "textureWithCGImage:options:error:") (retPtr retVoid) [argPtr cgImage, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ cubeMapWithContentsOfFiles:options:error:@
cubeMapWithContentsOfFiles_options_error :: (IsNSArray paths, IsNSDictionary options, IsNSError outError) => paths -> options -> outError -> IO (Id GLKTextureInfo)
cubeMapWithContentsOfFiles_options_error paths options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr paths $ \raw_paths ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "cubeMapWithContentsOfFiles:options:error:") (retPtr retVoid) [argPtr (castPtr raw_paths :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ cubeMapWithContentsOfFile:options:error:@
cubeMapWithContentsOfFile_options_error :: (IsNSString path, IsNSDictionary options, IsNSError outError) => path -> options -> outError -> IO (Id GLKTextureInfo)
cubeMapWithContentsOfFile_options_error path options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr path $ \raw_path ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "cubeMapWithContentsOfFile:options:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ cubeMapWithContentsOfURL:options:error:@
cubeMapWithContentsOfURL_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError outError) => url -> options -> outError -> IO (Id GLKTextureInfo)
cubeMapWithContentsOfURL_options_error url options outError =
  do
    cls' <- getRequiredClass "GLKTextureLoader"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "cubeMapWithContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithShareContext:@
initWithShareContext :: (IsGLKTextureLoader glkTextureLoader, IsNSOpenGLContext context) => glkTextureLoader -> context -> IO (Id GLKTextureLoader)
initWithShareContext glkTextureLoader  context =
withObjCPtr context $ \raw_context ->
    sendMsg glkTextureLoader (mkSelector "initWithShareContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= ownedObject . castPtr

-- | @- textureWithContentsOfFile:options:queue:completionHandler:@
textureWithContentsOfFile_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSString path, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> path -> options -> queue -> Ptr () -> IO ()
textureWithContentsOfFile_options_queue_completionHandler glkTextureLoader  path options queue block =
withObjCPtr path $ \raw_path ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg glkTextureLoader (mkSelector "textureWithContentsOfFile:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- textureWithContentsOfURL:options:queue:completionHandler:@
textureWithContentsOfURL_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSURL url, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> url -> options -> queue -> Ptr () -> IO ()
textureWithContentsOfURL_options_queue_completionHandler glkTextureLoader  url options queue block =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg glkTextureLoader (mkSelector "textureWithContentsOfURL:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- textureWithName:scaleFactor:bundle:options:queue:completionHandler:@
textureWithName_scaleFactor_bundle_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSString name, IsNSBundle bundle, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> name -> CDouble -> bundle -> options -> queue -> Ptr () -> IO ()
textureWithName_scaleFactor_bundle_options_queue_completionHandler glkTextureLoader  name scaleFactor bundle options queue block =
withObjCPtr name $ \raw_name ->
  withObjCPtr bundle $ \raw_bundle ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg glkTextureLoader (mkSelector "textureWithName:scaleFactor:bundle:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argCDouble (fromIntegral scaleFactor), argPtr (castPtr raw_bundle :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- textureWithContentsOfData:options:queue:completionHandler:@
textureWithContentsOfData_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSData data_, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> data_ -> options -> queue -> Ptr () -> IO ()
textureWithContentsOfData_options_queue_completionHandler glkTextureLoader  data_ options queue block =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg glkTextureLoader (mkSelector "textureWithContentsOfData:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- textureWithCGImage:options:queue:completionHandler:@
textureWithCGImage_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> Ptr () -> options -> queue -> Ptr () -> IO ()
textureWithCGImage_options_queue_completionHandler glkTextureLoader  cgImage options queue block =
withObjCPtr options $ \raw_options ->
  withObjCPtr queue $ \raw_queue ->
      sendMsg glkTextureLoader (mkSelector "textureWithCGImage:options:queue:completionHandler:") retVoid [argPtr cgImage, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- cubeMapWithContentsOfFiles:options:queue:completionHandler:@
cubeMapWithContentsOfFiles_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSArray paths, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> paths -> options -> queue -> Ptr () -> IO ()
cubeMapWithContentsOfFiles_options_queue_completionHandler glkTextureLoader  paths options queue block =
withObjCPtr paths $ \raw_paths ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg glkTextureLoader (mkSelector "cubeMapWithContentsOfFiles:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_paths :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- cubeMapWithContentsOfFile:options:queue:completionHandler:@
cubeMapWithContentsOfFile_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSString path, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> path -> options -> queue -> Ptr () -> IO ()
cubeMapWithContentsOfFile_options_queue_completionHandler glkTextureLoader  path options queue block =
withObjCPtr path $ \raw_path ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg glkTextureLoader (mkSelector "cubeMapWithContentsOfFile:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- cubeMapWithContentsOfURL:options:queue:completionHandler:@
cubeMapWithContentsOfURL_options_queue_completionHandler :: (IsGLKTextureLoader glkTextureLoader, IsNSURL url, IsNSDictionary options, IsNSObject queue) => glkTextureLoader -> url -> options -> queue -> Ptr () -> IO ()
cubeMapWithContentsOfURL_options_queue_completionHandler glkTextureLoader  url options queue block =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg glkTextureLoader (mkSelector "cubeMapWithContentsOfURL:options:queue:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textureWithContentsOfFile:options:error:@
textureWithContentsOfFile_options_errorSelector :: Selector
textureWithContentsOfFile_options_errorSelector = mkSelector "textureWithContentsOfFile:options:error:"

-- | @Selector@ for @textureWithContentsOfURL:options:error:@
textureWithContentsOfURL_options_errorSelector :: Selector
textureWithContentsOfURL_options_errorSelector = mkSelector "textureWithContentsOfURL:options:error:"

-- | @Selector@ for @textureWithName:scaleFactor:bundle:options:error:@
textureWithName_scaleFactor_bundle_options_errorSelector :: Selector
textureWithName_scaleFactor_bundle_options_errorSelector = mkSelector "textureWithName:scaleFactor:bundle:options:error:"

-- | @Selector@ for @textureWithContentsOfData:options:error:@
textureWithContentsOfData_options_errorSelector :: Selector
textureWithContentsOfData_options_errorSelector = mkSelector "textureWithContentsOfData:options:error:"

-- | @Selector@ for @textureWithCGImage:options:error:@
textureWithCGImage_options_errorSelector :: Selector
textureWithCGImage_options_errorSelector = mkSelector "textureWithCGImage:options:error:"

-- | @Selector@ for @cubeMapWithContentsOfFiles:options:error:@
cubeMapWithContentsOfFiles_options_errorSelector :: Selector
cubeMapWithContentsOfFiles_options_errorSelector = mkSelector "cubeMapWithContentsOfFiles:options:error:"

-- | @Selector@ for @cubeMapWithContentsOfFile:options:error:@
cubeMapWithContentsOfFile_options_errorSelector :: Selector
cubeMapWithContentsOfFile_options_errorSelector = mkSelector "cubeMapWithContentsOfFile:options:error:"

-- | @Selector@ for @cubeMapWithContentsOfURL:options:error:@
cubeMapWithContentsOfURL_options_errorSelector :: Selector
cubeMapWithContentsOfURL_options_errorSelector = mkSelector "cubeMapWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithShareContext:@
initWithShareContextSelector :: Selector
initWithShareContextSelector = mkSelector "initWithShareContext:"

-- | @Selector@ for @textureWithContentsOfFile:options:queue:completionHandler:@
textureWithContentsOfFile_options_queue_completionHandlerSelector :: Selector
textureWithContentsOfFile_options_queue_completionHandlerSelector = mkSelector "textureWithContentsOfFile:options:queue:completionHandler:"

-- | @Selector@ for @textureWithContentsOfURL:options:queue:completionHandler:@
textureWithContentsOfURL_options_queue_completionHandlerSelector :: Selector
textureWithContentsOfURL_options_queue_completionHandlerSelector = mkSelector "textureWithContentsOfURL:options:queue:completionHandler:"

-- | @Selector@ for @textureWithName:scaleFactor:bundle:options:queue:completionHandler:@
textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector :: Selector
textureWithName_scaleFactor_bundle_options_queue_completionHandlerSelector = mkSelector "textureWithName:scaleFactor:bundle:options:queue:completionHandler:"

-- | @Selector@ for @textureWithContentsOfData:options:queue:completionHandler:@
textureWithContentsOfData_options_queue_completionHandlerSelector :: Selector
textureWithContentsOfData_options_queue_completionHandlerSelector = mkSelector "textureWithContentsOfData:options:queue:completionHandler:"

-- | @Selector@ for @textureWithCGImage:options:queue:completionHandler:@
textureWithCGImage_options_queue_completionHandlerSelector :: Selector
textureWithCGImage_options_queue_completionHandlerSelector = mkSelector "textureWithCGImage:options:queue:completionHandler:"

-- | @Selector@ for @cubeMapWithContentsOfFiles:options:queue:completionHandler:@
cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector :: Selector
cubeMapWithContentsOfFiles_options_queue_completionHandlerSelector = mkSelector "cubeMapWithContentsOfFiles:options:queue:completionHandler:"

-- | @Selector@ for @cubeMapWithContentsOfFile:options:queue:completionHandler:@
cubeMapWithContentsOfFile_options_queue_completionHandlerSelector :: Selector
cubeMapWithContentsOfFile_options_queue_completionHandlerSelector = mkSelector "cubeMapWithContentsOfFile:options:queue:completionHandler:"

-- | @Selector@ for @cubeMapWithContentsOfURL:options:queue:completionHandler:@
cubeMapWithContentsOfURL_options_queue_completionHandlerSelector :: Selector
cubeMapWithContentsOfURL_options_queue_completionHandlerSelector = mkSelector "cubeMapWithContentsOfURL:options:queue:completionHandler:"

