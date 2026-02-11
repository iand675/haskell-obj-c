{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCDirectionPadElement@.
--
-- Usage:
--
-- @
-- delegate <- newGCDirectionPadElement defaultGCDirectionPadElementOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCDirectionPadElement
  ( GCDirectionPadElementOverrides(..)
  , defaultGCDirectionPadElementOverrides
  , newGCDirectionPadElement
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

-- | Overrides record for @\@protocol GCDirectionPadElement@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCDirectionPadElementOverrides = GCDirectionPadElementOverrides
  { _xyAxes :: !(Maybe (IO RawId))
  , _xAxis :: !(Maybe (IO RawId))
  , _yAxis :: !(Maybe (IO RawId))
  , _up :: !(Maybe (IO RawId))
  , _down :: !(Maybe (IO RawId))
  , _left :: !(Maybe (IO RawId))
  , _right :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCDirectionPadElementOverrides :: GCDirectionPadElementOverrides
defaultGCDirectionPadElementOverrides = GCDirectionPadElementOverrides
  { _xyAxes = Nothing
  , _xAxis = Nothing
  , _yAxis = Nothing
  , _up = Nothing
  , _down = Nothing
  , _left = Nothing
  , _right = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gcDirectionPadElementDelegateClass #-}
gcDirectionPadElementDelegateClass :: Class
gcDirectionPadElementDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCDirectionPadElement" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_xyAxes = unSelector (mkSelector "xyAxes")
      sel_xAxis = unSelector (mkSelector "xAxis")
      sel_yAxis = unSelector (mkSelector "yAxis")
      sel_up = unSelector (mkSelector "up")
      sel_down = unSelector (mkSelector "down")
      sel_left = unSelector (mkSelector "left")
      sel_right = unSelector (mkSelector "right")
  -- xyAxes
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _xyAxes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "xyAxes" "@@:" stub_0

  -- xAxis
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _xAxis rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "xAxis" "@@:" stub_1

  -- yAxis
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _yAxis rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "yAxis" "@@:" stub_2

  -- up
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _up rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "up" "@@:" stub_3

  -- down
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _down rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "down" "@@:" stub_4

  -- left
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _left rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "left" "@@:" stub_5

  -- right
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    case _right rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "right" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDirectionPadElementOverrides
    if queriedSel == sel_xyAxes then pure (maybe 0 (const 1) (_xyAxes rec_))
    else if queriedSel == sel_xAxis then pure (maybe 0 (const 1) (_xAxis rec_))
    else if queriedSel == sel_yAxis then pure (maybe 0 (const 1) (_yAxis rec_))
    else if queriedSel == sel_up then pure (maybe 0 (const 1) (_up rec_))
    else if queriedSel == sel_down then pure (maybe 0 (const 1) (_down rec_))
    else if queriedSel == sel_left then pure (maybe 0 (const 1) (_left rec_))
    else if queriedSel == sel_right then pure (maybe 0 (const 1) (_right rec_))
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
newGCDirectionPadElement :: GCDirectionPadElementOverrides -> IO RawId
newGCDirectionPadElement overrides = do
  inst <- class_createInstance gcDirectionPadElementDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
