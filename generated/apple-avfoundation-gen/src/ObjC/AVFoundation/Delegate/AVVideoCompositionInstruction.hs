{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVVideoCompositionInstruction@.
--
-- Usage:
--
-- @
-- delegate <- newAVVideoCompositionInstruction defaultAVVideoCompositionInstructionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVVideoCompositionInstruction
  ( AVVideoCompositionInstructionOverrides(..)
  , defaultAVVideoCompositionInstructionOverrides
  , newAVVideoCompositionInstruction
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

-- | Overrides record for @\@protocol AVVideoCompositionInstruction@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVVideoCompositionInstructionOverrides = AVVideoCompositionInstructionOverrides
  { _enablePostProcessing :: !(Maybe (IO Bool))
  , _containsTweening :: !(Maybe (IO Bool))
  , _requiredSourceTrackIDs :: !(Maybe (IO RawId))
  , _passthroughTrackID :: !(Maybe (IO Int))
  , _requiredSourceSampleDataTrackIDs :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultAVVideoCompositionInstructionOverrides :: AVVideoCompositionInstructionOverrides
defaultAVVideoCompositionInstructionOverrides = AVVideoCompositionInstructionOverrides
  { _enablePostProcessing = Nothing
  , _containsTweening = Nothing
  , _requiredSourceTrackIDs = Nothing
  , _passthroughTrackID = Nothing
  , _requiredSourceSampleDataTrackIDs = Nothing
  }

foreign import ccall "wrapper"
  wrap_i
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avVideoCompositionInstructionDelegateClass #-}
avVideoCompositionInstructionDelegateClass :: Class
avVideoCompositionInstructionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVVideoCompositionInstruction" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_enablePostProcessing = unSelector (mkSelector "enablePostProcessing")
      sel_containsTweening = unSelector (mkSelector "containsTweening")
      sel_requiredSourceTrackIDs = unSelector (mkSelector "requiredSourceTrackIDs")
      sel_passthroughTrackID = unSelector (mkSelector "passthroughTrackID")
      sel_requiredSourceSampleDataTrackIDs = unSelector (mkSelector "requiredSourceSampleDataTrackIDs")
  -- enablePostProcessing
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionInstructionOverrides
    case _enablePostProcessing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "enablePostProcessing" "B@:" stub_0

  -- containsTweening
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionInstructionOverrides
    case _containsTweening rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "containsTweening" "B@:" stub_1

  -- requiredSourceTrackIDs
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionInstructionOverrides
    case _requiredSourceTrackIDs rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "requiredSourceTrackIDs" "@@:" stub_2

  -- passthroughTrackID
  stub_3 <- wrap_i $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionInstructionOverrides
    case _passthroughTrackID rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "passthroughTrackID" "i@:" stub_3

  -- requiredSourceSampleDataTrackIDs
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionInstructionOverrides
    case _requiredSourceSampleDataTrackIDs rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "requiredSourceSampleDataTrackIDs" "@@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionInstructionOverrides
    if queriedSel == sel_enablePostProcessing then pure (maybe 0 (const 1) (_enablePostProcessing rec_))
    else if queriedSel == sel_containsTweening then pure (maybe 0 (const 1) (_containsTweening rec_))
    else if queriedSel == sel_requiredSourceTrackIDs then pure (maybe 0 (const 1) (_requiredSourceTrackIDs rec_))
    else if queriedSel == sel_passthroughTrackID then pure (maybe 0 (const 1) (_passthroughTrackID rec_))
    else if queriedSel == sel_requiredSourceSampleDataTrackIDs then pure (maybe 0 (const 1) (_requiredSourceSampleDataTrackIDs rec_))
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
newAVVideoCompositionInstruction :: AVVideoCompositionInstructionOverrides -> IO RawId
newAVVideoCompositionInstruction overrides = do
  inst <- class_createInstance avVideoCompositionInstructionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
