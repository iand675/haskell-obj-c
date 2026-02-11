{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAudioPlayerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVAudioPlayerDelegate defaultAVAudioPlayerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFAudio.Delegate.AVAudioPlayerDelegate
  ( AVAudioPlayerDelegateOverrides(..)
  , defaultAVAudioPlayerDelegateOverrides
  , newAVAudioPlayerDelegate
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

-- | Overrides record for @\@protocol AVAudioPlayerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAudioPlayerDelegateOverrides = AVAudioPlayerDelegateOverrides
  { _audioPlayerDidFinishPlaying_successfully :: !(Maybe (RawId -> Bool -> IO ()))
  , _audioPlayerDecodeErrorDidOccur_error :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAudioPlayerDelegateOverrides :: AVAudioPlayerDelegateOverrides
defaultAVAudioPlayerDelegateOverrides = AVAudioPlayerDelegateOverrides
  { _audioPlayerDidFinishPlaying_successfully = Nothing
  , _audioPlayerDecodeErrorDidOccur_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avAudioPlayerDelegateDelegateClass #-}
avAudioPlayerDelegateDelegateClass :: Class
avAudioPlayerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAudioPlayerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_audioPlayerDidFinishPlaying_successfully = unSelector (mkSelector "audioPlayerDidFinishPlaying:successfully:")
      sel_audioPlayerDecodeErrorDidOccur_error = unSelector (mkSelector "audioPlayerDecodeErrorDidOccur:error:")
  -- audioPlayerDidFinishPlaying:successfully:
  stub_0 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioPlayerDelegateOverrides
    case _audioPlayerDidFinishPlaying_successfully rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "audioPlayerDidFinishPlaying:successfully:" "v@:@B" stub_0

  -- audioPlayerDecodeErrorDidOccur:error:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioPlayerDelegateOverrides
    case _audioPlayerDecodeErrorDidOccur_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "audioPlayerDecodeErrorDidOccur:error:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioPlayerDelegateOverrides
    if queriedSel == sel_audioPlayerDidFinishPlaying_successfully then pure (maybe 0 (const 1) (_audioPlayerDidFinishPlaying_successfully rec_))
    else if queriedSel == sel_audioPlayerDecodeErrorDidOccur_error then pure (maybe 0 (const 1) (_audioPlayerDecodeErrorDidOccur_error rec_))
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
newAVAudioPlayerDelegate :: AVAudioPlayerDelegateOverrides -> IO RawId
newAVAudioPlayerDelegate overrides = do
  inst <- class_createInstance avAudioPlayerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
