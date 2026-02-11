{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IKSlideshowDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newIKSlideshowDataSource defaultIKSlideshowDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.IKSlideshowDataSource
  ( IKSlideshowDataSourceOverrides(..)
  , defaultIKSlideshowDataSourceOverrides
  , newIKSlideshowDataSource
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

-- | Overrides record for @\@protocol IKSlideshowDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IKSlideshowDataSourceOverrides = IKSlideshowDataSourceOverrides
  { _numberOfSlideshowItems :: !(Maybe (IO Int))
  , _slideshowItemAtIndex :: !(Maybe (Int -> IO RawId))
  , _nameOfSlideshowItemAtIndex :: !(Maybe (Int -> IO RawId))
  , _canExportSlideshowItemAtIndex_toApplication :: !(Maybe (Int -> RawId -> IO Bool))
  , _slideshowWillStart :: !(Maybe (IO ()))
  , _slideshowDidStop :: !(Maybe (IO ()))
  , _slideshowDidChangeCurrentIndex :: !(Maybe (Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIKSlideshowDataSourceOverrides :: IKSlideshowDataSourceOverrides
defaultIKSlideshowDataSourceOverrides = IKSlideshowDataSourceOverrides
  { _numberOfSlideshowItems = Nothing
  , _slideshowItemAtIndex = Nothing
  , _nameOfSlideshowItemAtIndex = Nothing
  , _canExportSlideshowItemAtIndex_toApplication = Nothing
  , _slideshowWillStart = Nothing
  , _slideshowDidStop = Nothing
  , _slideshowDidChangeCurrentIndex = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ikSlideshowDataSourceDelegateClass #-}
ikSlideshowDataSourceDelegateClass :: Class
ikSlideshowDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIKSlideshowDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_numberOfSlideshowItems = unSelector (mkSelector "numberOfSlideshowItems")
      sel_slideshowItemAtIndex = unSelector (mkSelector "slideshowItemAtIndex:")
      sel_nameOfSlideshowItemAtIndex = unSelector (mkSelector "nameOfSlideshowItemAtIndex:")
      sel_canExportSlideshowItemAtIndex_toApplication = unSelector (mkSelector "canExportSlideshowItemAtIndex:toApplication:")
      sel_slideshowWillStart = unSelector (mkSelector "slideshowWillStart")
      sel_slideshowDidStop = unSelector (mkSelector "slideshowDidStop")
      sel_slideshowDidChangeCurrentIndex = unSelector (mkSelector "slideshowDidChangeCurrentIndex:")
  -- numberOfSlideshowItems
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _numberOfSlideshowItems rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "numberOfSlideshowItems" "Q@:" stub_0

  -- slideshowItemAtIndex:
  stub_1 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _slideshowItemAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "slideshowItemAtIndex:" "@@:Q" stub_1

  -- nameOfSlideshowItemAtIndex:
  stub_2 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _nameOfSlideshowItemAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "nameOfSlideshowItemAtIndex:" "@@:Q" stub_2

  -- canExportSlideshowItemAtIndex:toApplication:
  stub_3 <- wrap_Q_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _canExportSlideshowItemAtIndex_toApplication rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "canExportSlideshowItemAtIndex:toApplication:" "B@:Q@" stub_3

  -- slideshowWillStart
  stub_4 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _slideshowWillStart rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "slideshowWillStart" "v@:" stub_4

  -- slideshowDidStop
  stub_5 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _slideshowDidStop rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "slideshowDidStop" "v@:" stub_5

  -- slideshowDidChangeCurrentIndex:
  stub_6 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    case _slideshowDidChangeCurrentIndex rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "slideshowDidChangeCurrentIndex:" "v@:Q" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKSlideshowDataSourceOverrides
    if queriedSel == sel_numberOfSlideshowItems then pure (maybe 0 (const 1) (_numberOfSlideshowItems rec_))
    else if queriedSel == sel_slideshowItemAtIndex then pure (maybe 0 (const 1) (_slideshowItemAtIndex rec_))
    else if queriedSel == sel_nameOfSlideshowItemAtIndex then pure (maybe 0 (const 1) (_nameOfSlideshowItemAtIndex rec_))
    else if queriedSel == sel_canExportSlideshowItemAtIndex_toApplication then pure (maybe 0 (const 1) (_canExportSlideshowItemAtIndex_toApplication rec_))
    else if queriedSel == sel_slideshowWillStart then pure (maybe 0 (const 1) (_slideshowWillStart rec_))
    else if queriedSel == sel_slideshowDidStop then pure (maybe 0 (const 1) (_slideshowDidStop rec_))
    else if queriedSel == sel_slideshowDidChangeCurrentIndex then pure (maybe 0 (const 1) (_slideshowDidChangeCurrentIndex rec_))
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
newIKSlideshowDataSource :: IKSlideshowDataSourceOverrides -> IO RawId
newIKSlideshowDataSource overrides = do
  inst <- class_createInstance ikSlideshowDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
