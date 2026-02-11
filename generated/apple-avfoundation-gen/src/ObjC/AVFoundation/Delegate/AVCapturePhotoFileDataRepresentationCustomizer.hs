{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVCapturePhotoFileDataRepresentationCustomizer@.
--
-- Usage:
--
-- @
-- delegate <- newAVCapturePhotoFileDataRepresentationCustomizer defaultAVCapturePhotoFileDataRepresentationCustomizerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVCapturePhotoFileDataRepresentationCustomizer
  ( AVCapturePhotoFileDataRepresentationCustomizerOverrides(..)
  , defaultAVCapturePhotoFileDataRepresentationCustomizerOverrides
  , newAVCapturePhotoFileDataRepresentationCustomizer
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol AVCapturePhotoFileDataRepresentationCustomizer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVCapturePhotoFileDataRepresentationCustomizerOverrides = AVCapturePhotoFileDataRepresentationCustomizerOverrides
  { _replacementMetadataForPhoto :: !(Maybe (RawId -> IO RawId))
  , _replacementDepthDataForPhoto :: !(Maybe (RawId -> IO RawId))
  , _replacementPortraitEffectsMatteForPhoto :: !(Maybe (RawId -> IO RawId))
  , _replacementSemanticSegmentationMatteOfType_forPhoto :: !(Maybe (RawId -> RawId -> IO RawId))
  , _replacementAppleProRAWCompressionSettingsForPhoto_defaultSettings_maximumBitDepth :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultAVCapturePhotoFileDataRepresentationCustomizerOverrides :: AVCapturePhotoFileDataRepresentationCustomizerOverrides
defaultAVCapturePhotoFileDataRepresentationCustomizerOverrides = AVCapturePhotoFileDataRepresentationCustomizerOverrides
  { _replacementMetadataForPhoto = Nothing
  , _replacementDepthDataForPhoto = Nothing
  , _replacementPortraitEffectsMatteForPhoto = Nothing
  , _replacementSemanticSegmentationMatteOfType_forPhoto = Nothing
  , _replacementAppleProRAWCompressionSettingsForPhoto_defaultSettings_maximumBitDepth = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avCapturePhotoFileDataRepresentationCustomizerDelegateClass #-}
avCapturePhotoFileDataRepresentationCustomizerDelegateClass :: Class
avCapturePhotoFileDataRepresentationCustomizerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVCapturePhotoFileDataRepresentationCustomizer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_replacementMetadataForPhoto = unSelector (mkSelector "replacementMetadataForPhoto:")
      sel_replacementDepthDataForPhoto = unSelector (mkSelector "replacementDepthDataForPhoto:")
      sel_replacementPortraitEffectsMatteForPhoto = unSelector (mkSelector "replacementPortraitEffectsMatteForPhoto:")
      sel_replacementSemanticSegmentationMatteOfType_forPhoto = unSelector (mkSelector "replacementSemanticSegmentationMatteOfType:forPhoto:")
      sel_replacementAppleProRAWCompressionSettingsForPhoto_defaultSettings_maximumBitDepth = unSelector (mkSelector "replacementAppleProRAWCompressionSettingsForPhoto:defaultSettings:maximumBitDepth:")
  -- replacementMetadataForPhoto:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoFileDataRepresentationCustomizerOverrides
    case _replacementMetadataForPhoto rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "replacementMetadataForPhoto:" "@@:@" stub_0

  -- replacementDepthDataForPhoto:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoFileDataRepresentationCustomizerOverrides
    case _replacementDepthDataForPhoto rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "replacementDepthDataForPhoto:" "@@:@" stub_1

  -- replacementPortraitEffectsMatteForPhoto:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoFileDataRepresentationCustomizerOverrides
    case _replacementPortraitEffectsMatteForPhoto rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "replacementPortraitEffectsMatteForPhoto:" "@@:@" stub_2

  -- replacementSemanticSegmentationMatteOfType:forPhoto:
  stub_3 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoFileDataRepresentationCustomizerOverrides
    case _replacementSemanticSegmentationMatteOfType_forPhoto rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "replacementSemanticSegmentationMatteOfType:forPhoto:" "@@:@@" stub_3

  -- replacementAppleProRAWCompressionSettingsForPhoto:defaultSettings:maximumBitDepth:
  stub_4 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoFileDataRepresentationCustomizerOverrides
    case _replacementAppleProRAWCompressionSettingsForPhoto_defaultSettings_maximumBitDepth rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "replacementAppleProRAWCompressionSettingsForPhoto:defaultSettings:maximumBitDepth:" "@@:@@q" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoFileDataRepresentationCustomizerOverrides
    if queriedSel == sel_replacementMetadataForPhoto then pure (maybe 0 (const 1) (_replacementMetadataForPhoto rec_))
    else if queriedSel == sel_replacementDepthDataForPhoto then pure (maybe 0 (const 1) (_replacementDepthDataForPhoto rec_))
    else if queriedSel == sel_replacementPortraitEffectsMatteForPhoto then pure (maybe 0 (const 1) (_replacementPortraitEffectsMatteForPhoto rec_))
    else if queriedSel == sel_replacementSemanticSegmentationMatteOfType_forPhoto then pure (maybe 0 (const 1) (_replacementSemanticSegmentationMatteOfType_forPhoto rec_))
    else if queriedSel == sel_replacementAppleProRAWCompressionSettingsForPhoto_defaultSettings_maximumBitDepth then pure (maybe 0 (const 1) (_replacementAppleProRAWCompressionSettingsForPhoto_defaultSettings_maximumBitDepth rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newAVCapturePhotoFileDataRepresentationCustomizer :: AVCapturePhotoFileDataRepresentationCustomizerOverrides -> IO RawId
newAVCapturePhotoFileDataRepresentationCustomizer overrides = do
  inst <- class_createInstance avCapturePhotoFileDataRepresentationCustomizerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
