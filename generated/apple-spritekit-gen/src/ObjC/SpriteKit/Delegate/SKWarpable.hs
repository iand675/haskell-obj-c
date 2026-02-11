{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SKWarpable@.
--
-- Usage:
--
-- @
-- delegate <- newSKWarpable defaultSKWarpableOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SpriteKit.Delegate.SKWarpable
  ( SKWarpableOverrides(..)
  , defaultSKWarpableOverrides
  , newSKWarpable
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

-- | Overrides record for @\@protocol SKWarpable@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SKWarpableOverrides = SKWarpableOverrides
  { _warpGeometry :: !(Maybe (IO RawId))
  , _setWarpGeometry :: !(Maybe (RawId -> IO ()))
  , _subdivisionLevels :: !(Maybe (IO Int))
  , _setSubdivisionLevels :: !(Maybe (Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSKWarpableOverrides :: SKWarpableOverrides
defaultSKWarpableOverrides = SKWarpableOverrides
  { _warpGeometry = Nothing
  , _setWarpGeometry = Nothing
  , _subdivisionLevels = Nothing
  , _setSubdivisionLevels = Nothing
  }

foreign import ccall "wrapper"
  wrap_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE skWarpableDelegateClass #-}
skWarpableDelegateClass :: Class
skWarpableDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSKWarpable" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_warpGeometry = unSelector (mkSelector "warpGeometry")
      sel_setWarpGeometry = unSelector (mkSelector "setWarpGeometry:")
      sel_subdivisionLevels = unSelector (mkSelector "subdivisionLevels")
      sel_setSubdivisionLevels = unSelector (mkSelector "setSubdivisionLevels:")
  -- warpGeometry
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKWarpableOverrides
    case _warpGeometry rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "warpGeometry" "@@:" stub_0

  -- setWarpGeometry:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKWarpableOverrides
    case _setWarpGeometry rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setWarpGeometry:" "v@:@" stub_1

  -- subdivisionLevels
  stub_2 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKWarpableOverrides
    case _subdivisionLevels rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "subdivisionLevels" "q@:" stub_2

  -- setSubdivisionLevels:
  stub_3 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKWarpableOverrides
    case _setSubdivisionLevels rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setSubdivisionLevels:" "v@:q" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKWarpableOverrides
    if queriedSel == sel_warpGeometry then pure (maybe 0 (const 1) (_warpGeometry rec_))
    else if queriedSel == sel_setWarpGeometry then pure (maybe 0 (const 1) (_setWarpGeometry rec_))
    else if queriedSel == sel_subdivisionLevels then pure (maybe 0 (const 1) (_subdivisionLevels rec_))
    else if queriedSel == sel_setSubdivisionLevels then pure (maybe 0 (const 1) (_setSubdivisionLevels rec_))
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
newSKWarpable :: SKWarpableOverrides -> IO RawId
newSKWarpable overrides = do
  inst <- class_createInstance skWarpableDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
