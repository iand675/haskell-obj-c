{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSKeyedUnarchiverDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSKeyedUnarchiverDelegate defaultNSKeyedUnarchiverDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSKeyedUnarchiverDelegate
  ( NSKeyedUnarchiverDelegateOverrides(..)
  , defaultNSKeyedUnarchiverDelegateOverrides
  , newNSKeyedUnarchiverDelegate
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

-- | Overrides record for @\@protocol NSKeyedUnarchiverDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSKeyedUnarchiverDelegateOverrides = NSKeyedUnarchiverDelegateOverrides
  { _unarchiver_cannotDecodeObjectOfClassName_originalClasses :: !(Maybe (RawId -> RawId -> RawId -> IO Class))
  , _unarchiver_didDecodeObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _unarchiver_willReplaceObject_withObject :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _unarchiverWillFinish :: !(Maybe (RawId -> IO ()))
  , _unarchiverDidFinish :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSKeyedUnarchiverDelegateOverrides :: NSKeyedUnarchiverDelegateOverrides
defaultNSKeyedUnarchiverDelegateOverrides = NSKeyedUnarchiverDelegateOverrides
  { _unarchiver_cannotDecodeObjectOfClassName_originalClasses = Nothing
  , _unarchiver_didDecodeObject = Nothing
  , _unarchiver_willReplaceObject_withObject = Nothing
  , _unarchiverWillFinish = Nothing
  , _unarchiverDidFinish = Nothing
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
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_cls
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsKeyedUnarchiverDelegateDelegateClass #-}
nsKeyedUnarchiverDelegateDelegateClass :: Class
nsKeyedUnarchiverDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSKeyedUnarchiverDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_unarchiver_cannotDecodeObjectOfClassName_originalClasses = unSelector (mkSelector "unarchiver:cannotDecodeObjectOfClassName:originalClasses:")
      sel_unarchiver_didDecodeObject = unSelector (mkSelector "unarchiver:didDecodeObject:")
      sel_unarchiver_willReplaceObject_withObject = unSelector (mkSelector "unarchiver:willReplaceObject:withObject:")
      sel_unarchiverWillFinish = unSelector (mkSelector "unarchiverWillFinish:")
      sel_unarchiverDidFinish = unSelector (mkSelector "unarchiverDidFinish:")
  -- unarchiver:cannotDecodeObjectOfClassName:originalClasses:
  stub_0 <- wrap_at_at_at_cls $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedUnarchiverDelegateOverrides
    case _unarchiver_cannotDecodeObjectOfClassName_originalClasses rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unClass r) :: Ptr ObjCObject)
  addObjCMethod cls "unarchiver:cannotDecodeObjectOfClassName:originalClasses:" "#@:@@@" stub_0

  -- unarchiver:didDecodeObject:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedUnarchiverDelegateOverrides
    case _unarchiver_didDecodeObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "unarchiver:didDecodeObject:" "@@:@@" stub_1

  -- unarchiver:willReplaceObject:withObject:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedUnarchiverDelegateOverrides
    case _unarchiver_willReplaceObject_withObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "unarchiver:willReplaceObject:withObject:" "v@:@@@" stub_2

  -- unarchiverWillFinish:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedUnarchiverDelegateOverrides
    case _unarchiverWillFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "unarchiverWillFinish:" "v@:@" stub_3

  -- unarchiverDidFinish:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedUnarchiverDelegateOverrides
    case _unarchiverDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "unarchiverDidFinish:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSKeyedUnarchiverDelegateOverrides
    if queriedSel == sel_unarchiver_cannotDecodeObjectOfClassName_originalClasses then pure (maybe 0 (const 1) (_unarchiver_cannotDecodeObjectOfClassName_originalClasses rec_))
    else if queriedSel == sel_unarchiver_didDecodeObject then pure (maybe 0 (const 1) (_unarchiver_didDecodeObject rec_))
    else if queriedSel == sel_unarchiver_willReplaceObject_withObject then pure (maybe 0 (const 1) (_unarchiver_willReplaceObject_withObject rec_))
    else if queriedSel == sel_unarchiverWillFinish then pure (maybe 0 (const 1) (_unarchiverWillFinish rec_))
    else if queriedSel == sel_unarchiverDidFinish then pure (maybe 0 (const 1) (_unarchiverDidFinish rec_))
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
newNSKeyedUnarchiverDelegate :: NSKeyedUnarchiverDelegateOverrides -> IO RawId
newNSKeyedUnarchiverDelegate overrides = do
  inst <- class_createInstance nsKeyedUnarchiverDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
