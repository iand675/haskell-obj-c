{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCRenderer@.
module ObjC.Quartz.QCRenderer
  ( QCRenderer
  , IsQCRenderer(..)
  , initWithComposition_colorSpace
  , initWithCGLContext_pixelFormat_colorSpace_composition
  , initOffScreenWithSize_colorSpace_composition
  , initWithOpenGLContext_pixelFormat_file
  , renderAtTime_arguments
  , renderingTimeForTime_arguments
  , composition
  , snapshotImage
  , createSnapshotImageOfType
  , compositionSelector
  , createSnapshotImageOfTypeSelector
  , initOffScreenWithSize_colorSpace_compositionSelector
  , initWithCGLContext_pixelFormat_colorSpace_compositionSelector
  , initWithComposition_colorSpaceSelector
  , initWithOpenGLContext_pixelFormat_fileSelector
  , renderAtTime_argumentsSelector
  , renderingTimeForTime_argumentsSelector
  , snapshotImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithComposition:colorSpace:@
initWithComposition_colorSpace :: (IsQCRenderer qcRenderer, IsQCComposition composition) => qcRenderer -> composition -> Ptr () -> IO RawId
initWithComposition_colorSpace qcRenderer composition colorSpace =
  sendOwnedMessage qcRenderer initWithComposition_colorSpaceSelector (toQCComposition composition) colorSpace

-- | @- initWithCGLContext:pixelFormat:colorSpace:composition:@
initWithCGLContext_pixelFormat_colorSpace_composition :: (IsQCRenderer qcRenderer, IsQCComposition composition) => qcRenderer -> Ptr () -> Ptr () -> Ptr () -> composition -> IO RawId
initWithCGLContext_pixelFormat_colorSpace_composition qcRenderer context format colorSpace composition =
  sendOwnedMessage qcRenderer initWithCGLContext_pixelFormat_colorSpace_compositionSelector context format colorSpace (toQCComposition composition)

-- | @- initOffScreenWithSize:colorSpace:composition:@
initOffScreenWithSize_colorSpace_composition :: (IsQCRenderer qcRenderer, IsQCComposition composition) => qcRenderer -> NSSize -> Ptr () -> composition -> IO RawId
initOffScreenWithSize_colorSpace_composition qcRenderer size colorSpace composition =
  sendOwnedMessage qcRenderer initOffScreenWithSize_colorSpace_compositionSelector size colorSpace (toQCComposition composition)

-- | @- initWithOpenGLContext:pixelFormat:file:@
initWithOpenGLContext_pixelFormat_file :: (IsQCRenderer qcRenderer, IsNSOpenGLContext context, IsNSOpenGLPixelFormat format, IsNSString path) => qcRenderer -> context -> format -> path -> IO RawId
initWithOpenGLContext_pixelFormat_file qcRenderer context format path =
  sendOwnedMessage qcRenderer initWithOpenGLContext_pixelFormat_fileSelector (toNSOpenGLContext context) (toNSOpenGLPixelFormat format) (toNSString path)

-- | @- renderAtTime:arguments:@
renderAtTime_arguments :: (IsQCRenderer qcRenderer, IsNSDictionary arguments) => qcRenderer -> CDouble -> arguments -> IO Bool
renderAtTime_arguments qcRenderer time arguments =
  sendMessage qcRenderer renderAtTime_argumentsSelector time (toNSDictionary arguments)

-- | @- renderingTimeForTime:arguments:@
renderingTimeForTime_arguments :: (IsQCRenderer qcRenderer, IsNSDictionary arguments) => qcRenderer -> CDouble -> arguments -> IO CDouble
renderingTimeForTime_arguments qcRenderer time arguments =
  sendMessage qcRenderer renderingTimeForTime_argumentsSelector time (toNSDictionary arguments)

-- | @- composition@
composition :: IsQCRenderer qcRenderer => qcRenderer -> IO (Id QCComposition)
composition qcRenderer =
  sendMessage qcRenderer compositionSelector

-- | @- snapshotImage@
snapshotImage :: IsQCRenderer qcRenderer => qcRenderer -> IO (Id NSImage)
snapshotImage qcRenderer =
  sendMessage qcRenderer snapshotImageSelector

-- | @- createSnapshotImageOfType:@
createSnapshotImageOfType :: (IsQCRenderer qcRenderer, IsNSString type_) => qcRenderer -> type_ -> IO RawId
createSnapshotImageOfType qcRenderer type_ =
  sendMessage qcRenderer createSnapshotImageOfTypeSelector (toNSString type_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithComposition:colorSpace:@
initWithComposition_colorSpaceSelector :: Selector '[Id QCComposition, Ptr ()] RawId
initWithComposition_colorSpaceSelector = mkSelector "initWithComposition:colorSpace:"

-- | @Selector@ for @initWithCGLContext:pixelFormat:colorSpace:composition:@
initWithCGLContext_pixelFormat_colorSpace_compositionSelector :: Selector '[Ptr (), Ptr (), Ptr (), Id QCComposition] RawId
initWithCGLContext_pixelFormat_colorSpace_compositionSelector = mkSelector "initWithCGLContext:pixelFormat:colorSpace:composition:"

-- | @Selector@ for @initOffScreenWithSize:colorSpace:composition:@
initOffScreenWithSize_colorSpace_compositionSelector :: Selector '[NSSize, Ptr (), Id QCComposition] RawId
initOffScreenWithSize_colorSpace_compositionSelector = mkSelector "initOffScreenWithSize:colorSpace:composition:"

-- | @Selector@ for @initWithOpenGLContext:pixelFormat:file:@
initWithOpenGLContext_pixelFormat_fileSelector :: Selector '[Id NSOpenGLContext, Id NSOpenGLPixelFormat, Id NSString] RawId
initWithOpenGLContext_pixelFormat_fileSelector = mkSelector "initWithOpenGLContext:pixelFormat:file:"

-- | @Selector@ for @renderAtTime:arguments:@
renderAtTime_argumentsSelector :: Selector '[CDouble, Id NSDictionary] Bool
renderAtTime_argumentsSelector = mkSelector "renderAtTime:arguments:"

-- | @Selector@ for @renderingTimeForTime:arguments:@
renderingTimeForTime_argumentsSelector :: Selector '[CDouble, Id NSDictionary] CDouble
renderingTimeForTime_argumentsSelector = mkSelector "renderingTimeForTime:arguments:"

-- | @Selector@ for @composition@
compositionSelector :: Selector '[] (Id QCComposition)
compositionSelector = mkSelector "composition"

-- | @Selector@ for @snapshotImage@
snapshotImageSelector :: Selector '[] (Id NSImage)
snapshotImageSelector = mkSelector "snapshotImage"

-- | @Selector@ for @createSnapshotImageOfType:@
createSnapshotImageOfTypeSelector :: Selector '[Id NSString] RawId
createSnapshotImageOfTypeSelector = mkSelector "createSnapshotImageOfType:"

