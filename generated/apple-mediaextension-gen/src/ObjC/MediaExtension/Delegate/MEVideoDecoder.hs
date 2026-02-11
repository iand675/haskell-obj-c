{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MEVideoDecoder@.
--
-- Usage:
--
-- @
-- delegate <- newMEVideoDecoder defaultMEVideoDecoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaExtension.Delegate.MEVideoDecoder
  ( MEVideoDecoderOverrides(..)
  , defaultMEVideoDecoderOverrides
  , newMEVideoDecoder
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

-- | Overrides record for @\@protocol MEVideoDecoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MEVideoDecoderOverrides = MEVideoDecoderOverrides
  { _canAcceptFormatDescription :: !(Maybe (RawId -> IO Bool))
  , _producesRAWOutput :: !(Maybe (IO Bool))
  , _contentHasInterframeDependencies :: !(Maybe (IO Bool))
  , _recommendedThreadCount :: !(Maybe (IO Int))
  , _setRecommendedThreadCount :: !(Maybe (Int -> IO ()))
  , _actualThreadCount :: !(Maybe (IO Int))
  , _supportedPixelFormatsOrderedByQuality :: !(Maybe (IO RawId))
  , _pixelFormatsWithReducedResolutionDecodeSupport :: !(Maybe (IO RawId))
  , _readyForMoreMediaData :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMEVideoDecoderOverrides :: MEVideoDecoderOverrides
defaultMEVideoDecoderOverrides = MEVideoDecoderOverrides
  { _canAcceptFormatDescription = Nothing
  , _producesRAWOutput = Nothing
  , _contentHasInterframeDependencies = Nothing
  , _recommendedThreadCount = Nothing
  , _setRecommendedThreadCount = Nothing
  , _actualThreadCount = Nothing
  , _supportedPixelFormatsOrderedByQuality = Nothing
  , _pixelFormatsWithReducedResolutionDecodeSupport = Nothing
  , _readyForMoreMediaData = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meVideoDecoderDelegateClass #-}
meVideoDecoderDelegateClass :: Class
meVideoDecoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMEVideoDecoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_canAcceptFormatDescription = unSelector (mkSelector "canAcceptFormatDescription:")
      sel_producesRAWOutput = unSelector (mkSelector "producesRAWOutput")
      sel_contentHasInterframeDependencies = unSelector (mkSelector "contentHasInterframeDependencies")
      sel_recommendedThreadCount = unSelector (mkSelector "recommendedThreadCount")
      sel_setRecommendedThreadCount = unSelector (mkSelector "setRecommendedThreadCount:")
      sel_actualThreadCount = unSelector (mkSelector "actualThreadCount")
      sel_supportedPixelFormatsOrderedByQuality = unSelector (mkSelector "supportedPixelFormatsOrderedByQuality")
      sel_pixelFormatsWithReducedResolutionDecodeSupport = unSelector (mkSelector "pixelFormatsWithReducedResolutionDecodeSupport")
      sel_readyForMoreMediaData = unSelector (mkSelector "readyForMoreMediaData")
  -- canAcceptFormatDescription:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _canAcceptFormatDescription rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "canAcceptFormatDescription:" "B@:@" stub_0

  -- producesRAWOutput
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _producesRAWOutput rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "producesRAWOutput" "B@:" stub_1

  -- contentHasInterframeDependencies
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _contentHasInterframeDependencies rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "contentHasInterframeDependencies" "B@:" stub_2

  -- recommendedThreadCount
  stub_3 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _recommendedThreadCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "recommendedThreadCount" "q@:" stub_3

  -- setRecommendedThreadCount:
  stub_4 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _setRecommendedThreadCount rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setRecommendedThreadCount:" "v@:q" stub_4

  -- actualThreadCount
  stub_5 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _actualThreadCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "actualThreadCount" "q@:" stub_5

  -- supportedPixelFormatsOrderedByQuality
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _supportedPixelFormatsOrderedByQuality rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "supportedPixelFormatsOrderedByQuality" "@@:" stub_6

  -- pixelFormatsWithReducedResolutionDecodeSupport
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _pixelFormatsWithReducedResolutionDecodeSupport rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pixelFormatsWithReducedResolutionDecodeSupport" "@@:" stub_7

  -- readyForMoreMediaData
  stub_8 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    case _readyForMoreMediaData rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "readyForMoreMediaData" "B@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEVideoDecoderOverrides
    if queriedSel == sel_canAcceptFormatDescription then pure (maybe 0 (const 1) (_canAcceptFormatDescription rec_))
    else if queriedSel == sel_producesRAWOutput then pure (maybe 0 (const 1) (_producesRAWOutput rec_))
    else if queriedSel == sel_contentHasInterframeDependencies then pure (maybe 0 (const 1) (_contentHasInterframeDependencies rec_))
    else if queriedSel == sel_recommendedThreadCount then pure (maybe 0 (const 1) (_recommendedThreadCount rec_))
    else if queriedSel == sel_setRecommendedThreadCount then pure (maybe 0 (const 1) (_setRecommendedThreadCount rec_))
    else if queriedSel == sel_actualThreadCount then pure (maybe 0 (const 1) (_actualThreadCount rec_))
    else if queriedSel == sel_supportedPixelFormatsOrderedByQuality then pure (maybe 0 (const 1) (_supportedPixelFormatsOrderedByQuality rec_))
    else if queriedSel == sel_pixelFormatsWithReducedResolutionDecodeSupport then pure (maybe 0 (const 1) (_pixelFormatsWithReducedResolutionDecodeSupport rec_))
    else if queriedSel == sel_readyForMoreMediaData then pure (maybe 0 (const 1) (_readyForMoreMediaData rec_))
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
newMEVideoDecoder :: MEVideoDecoderOverrides -> IO RawId
newMEVideoDecoder overrides = do
  inst <- class_createInstance meVideoDecoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
