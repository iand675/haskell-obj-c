{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPPlayableContentDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newMPPlayableContentDataSource defaultMPPlayableContentDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaPlayer.Delegate.MPPlayableContentDataSource
  ( MPPlayableContentDataSourceOverrides(..)
  , defaultMPPlayableContentDataSourceOverrides
  , newMPPlayableContentDataSource
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

-- | Overrides record for @\@protocol MPPlayableContentDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPPlayableContentDataSourceOverrides = MPPlayableContentDataSourceOverrides
  { _childItemsDisplayPlaybackProgressAtIndexPath :: !(Maybe (RawId -> IO Bool))
  , _numberOfChildItemsAtIndexPath :: !(Maybe (RawId -> IO Int))
  , _contentItemAtIndexPath :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMPPlayableContentDataSourceOverrides :: MPPlayableContentDataSourceOverrides
defaultMPPlayableContentDataSourceOverrides = MPPlayableContentDataSourceOverrides
  { _childItemsDisplayPlaybackProgressAtIndexPath = Nothing
  , _numberOfChildItemsAtIndexPath = Nothing
  , _contentItemAtIndexPath = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mpPlayableContentDataSourceDelegateClass #-}
mpPlayableContentDataSourceDelegateClass :: Class
mpPlayableContentDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPPlayableContentDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_childItemsDisplayPlaybackProgressAtIndexPath = unSelector (mkSelector "childItemsDisplayPlaybackProgressAtIndexPath:")
      sel_numberOfChildItemsAtIndexPath = unSelector (mkSelector "numberOfChildItemsAtIndexPath:")
      sel_contentItemAtIndexPath = unSelector (mkSelector "contentItemAtIndexPath:")
  -- childItemsDisplayPlaybackProgressAtIndexPath:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPPlayableContentDataSourceOverrides
    case _childItemsDisplayPlaybackProgressAtIndexPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "childItemsDisplayPlaybackProgressAtIndexPath:" "B@:@" stub_0

  -- numberOfChildItemsAtIndexPath:
  stub_1 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPPlayableContentDataSourceOverrides
    case _numberOfChildItemsAtIndexPath rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "numberOfChildItemsAtIndexPath:" "q@:@" stub_1

  -- contentItemAtIndexPath:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPPlayableContentDataSourceOverrides
    case _contentItemAtIndexPath rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentItemAtIndexPath:" "@@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPPlayableContentDataSourceOverrides
    if queriedSel == sel_childItemsDisplayPlaybackProgressAtIndexPath then pure (maybe 0 (const 1) (_childItemsDisplayPlaybackProgressAtIndexPath rec_))
    else if queriedSel == sel_numberOfChildItemsAtIndexPath then pure (maybe 0 (const 1) (_numberOfChildItemsAtIndexPath rec_))
    else if queriedSel == sel_contentItemAtIndexPath then pure (maybe 0 (const 1) (_contentItemAtIndexPath rec_))
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
newMPPlayableContentDataSource :: MPPlayableContentDataSourceOverrides -> IO RawId
newMPPlayableContentDataSource overrides = do
  inst <- class_createInstance mpPlayableContentDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
