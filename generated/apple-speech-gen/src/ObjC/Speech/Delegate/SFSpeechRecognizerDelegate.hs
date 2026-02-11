{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SFSpeechRecognizerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSFSpeechRecognizerDelegate defaultSFSpeechRecognizerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Speech.Delegate.SFSpeechRecognizerDelegate
  ( SFSpeechRecognizerDelegateOverrides(..)
  , defaultSFSpeechRecognizerDelegateOverrides
  , newSFSpeechRecognizerDelegate
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

-- | Overrides record for @\@protocol SFSpeechRecognizerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SFSpeechRecognizerDelegateOverrides = SFSpeechRecognizerDelegateOverrides
  { _speechRecognizer_availabilityDidChange :: !(Maybe (RawId -> Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSFSpeechRecognizerDelegateOverrides :: SFSpeechRecognizerDelegateOverrides
defaultSFSpeechRecognizerDelegateOverrides = SFSpeechRecognizerDelegateOverrides
  { _speechRecognizer_availabilityDidChange = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE sfSpeechRecognizerDelegateDelegateClass #-}
sfSpeechRecognizerDelegateDelegateClass :: Class
sfSpeechRecognizerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSFSpeechRecognizerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_speechRecognizer_availabilityDidChange = unSelector (mkSelector "speechRecognizer:availabilityDidChange:")
  -- speechRecognizer:availabilityDidChange:
  stub_0 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSpeechRecognizerDelegateOverrides
    case _speechRecognizer_availabilityDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "speechRecognizer:availabilityDidChange:" "v@:@B" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SFSpeechRecognizerDelegateOverrides
    if queriedSel == sel_speechRecognizer_availabilityDidChange then pure (maybe 0 (const 1) (_speechRecognizer_availabilityDidChange rec_))
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
newSFSpeechRecognizerDelegate :: SFSpeechRecognizerDelegateOverrides -> IO RawId
newSFSpeechRecognizerDelegate overrides = do
  inst <- class_createInstance sfSpeechRecognizerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
