{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSKeyedArchiverDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSKeyedArchiverDelegate defaultNSKeyedArchiverDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSKeyedArchiverDelegate
  ( NSKeyedArchiverDelegateOverrides(..)
  , defaultNSKeyedArchiverDelegateOverrides
  , newNSKeyedArchiverDelegate
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

-- | Overrides record for @\@protocol NSKeyedArchiverDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSKeyedArchiverDelegateOverrides = NSKeyedArchiverDelegateOverrides
  { _archiver_willEncodeObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _archiver_didEncodeObject :: !(Maybe (RawId -> RawId -> IO ()))
  , _archiver_willReplaceObject_withObject :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _archiverWillFinish :: !(Maybe (RawId -> IO ()))
  , _archiverDidFinish :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSKeyedArchiverDelegateOverrides :: NSKeyedArchiverDelegateOverrides
defaultNSKeyedArchiverDelegateOverrides = NSKeyedArchiverDelegateOverrides
  { _archiver_willEncodeObject = Nothing
  , _archiver_didEncodeObject = Nothing
  , _archiver_willReplaceObject_withObject = Nothing
  , _archiverWillFinish = Nothing
  , _archiverDidFinish = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsKeyedArchiverDelegateDelegateClass #-}
nsKeyedArchiverDelegateDelegateClass :: Class
nsKeyedArchiverDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSKeyedArchiverDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_archiver_willEncodeObject = unSelector (mkSelector "archiver:willEncodeObject:")
      sel_archiver_didEncodeObject = unSelector (mkSelector "archiver:didEncodeObject:")
      sel_archiver_willReplaceObject_withObject = unSelector (mkSelector "archiver:willReplaceObject:withObject:")
      sel_archiverWillFinish = unSelector (mkSelector "archiverWillFinish:")
      sel_archiverDidFinish = unSelector (mkSelector "archiverDidFinish:")
  -- archiver:willEncodeObject:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedArchiverDelegateOverrides
    case _archiver_willEncodeObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "archiver:willEncodeObject:" "@@:@@" stub_0

  -- archiver:didEncodeObject:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedArchiverDelegateOverrides
    case _archiver_didEncodeObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "archiver:didEncodeObject:" "v@:@@" stub_1

  -- archiver:willReplaceObject:withObject:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedArchiverDelegateOverrides
    case _archiver_willReplaceObject_withObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "archiver:willReplaceObject:withObject:" "v@:@@@" stub_2

  -- archiverWillFinish:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedArchiverDelegateOverrides
    case _archiverWillFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "archiverWillFinish:" "v@:@" stub_3

  -- archiverDidFinish:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedArchiverDelegateOverrides
    case _archiverDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "archiverDidFinish:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedArchiverDelegateOverrides
    if queriedSel == sel_archiver_willEncodeObject then pure (maybe 0 (const 1) (_archiver_willEncodeObject rec_))
    else if queriedSel == sel_archiver_didEncodeObject then pure (maybe 0 (const 1) (_archiver_didEncodeObject rec_))
    else if queriedSel == sel_archiver_willReplaceObject_withObject then pure (maybe 0 (const 1) (_archiver_willReplaceObject_withObject rec_))
    else if queriedSel == sel_archiverWillFinish then pure (maybe 0 (const 1) (_archiverWillFinish rec_))
    else if queriedSel == sel_archiverDidFinish then pure (maybe 0 (const 1) (_archiverDidFinish rec_))
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
newNSKeyedArchiverDelegate :: NSKeyedArchiverDelegateOverrides -> IO RawId
newNSKeyedArchiverDelegate overrides = do
  inst <- class_createInstance nsKeyedArchiverDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
