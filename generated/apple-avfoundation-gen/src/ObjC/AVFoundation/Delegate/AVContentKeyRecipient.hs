{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVContentKeyRecipient@.
--
-- Usage:
--
-- @
-- delegate <- newAVContentKeyRecipient defaultAVContentKeyRecipientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVContentKeyRecipient
  ( AVContentKeyRecipientOverrides(..)
  , defaultAVContentKeyRecipientOverrides
  , newAVContentKeyRecipient
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

-- | Overrides record for @\@protocol AVContentKeyRecipient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVContentKeyRecipientOverrides = AVContentKeyRecipientOverrides
  { _contentKeySession_didProvideContentKey :: !(Maybe (RawId -> RawId -> IO ()))
  , _mayRequireContentKeysForMediaDataProcessing :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultAVContentKeyRecipientOverrides :: AVContentKeyRecipientOverrides
defaultAVContentKeyRecipientOverrides = AVContentKeyRecipientOverrides
  { _contentKeySession_didProvideContentKey = Nothing
  , _mayRequireContentKeysForMediaDataProcessing = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avContentKeyRecipientDelegateClass #-}
avContentKeyRecipientDelegateClass :: Class
avContentKeyRecipientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVContentKeyRecipient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_contentKeySession_didProvideContentKey = unSelector (mkSelector "contentKeySession:didProvideContentKey:")
      sel_mayRequireContentKeysForMediaDataProcessing = unSelector (mkSelector "mayRequireContentKeysForMediaDataProcessing")
  -- contentKeySession:didProvideContentKey:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeyRecipientOverrides
    case _contentKeySession_didProvideContentKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contentKeySession:didProvideContentKey:" "v@:@@" stub_0

  -- mayRequireContentKeysForMediaDataProcessing
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeyRecipientOverrides
    case _mayRequireContentKeysForMediaDataProcessing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "mayRequireContentKeysForMediaDataProcessing" "B@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVContentKeyRecipientOverrides
    if queriedSel == sel_contentKeySession_didProvideContentKey then pure (maybe 0 (const 1) (_contentKeySession_didProvideContentKey rec_))
    else if queriedSel == sel_mayRequireContentKeysForMediaDataProcessing then pure (maybe 0 (const 1) (_mayRequireContentKeysForMediaDataProcessing rec_))
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
newAVContentKeyRecipient :: AVContentKeyRecipientOverrides -> IO RawId
newAVContentKeyRecipient overrides = do
  inst <- class_createInstance avContentKeyRecipientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
