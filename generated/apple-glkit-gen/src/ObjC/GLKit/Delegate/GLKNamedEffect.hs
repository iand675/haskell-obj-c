{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GLKNamedEffect@.
--
-- Usage:
--
-- @
-- delegate <- newGLKNamedEffect defaultGLKNamedEffectOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GLKit.Delegate.GLKNamedEffect
  ( GLKNamedEffectOverrides(..)
  , defaultGLKNamedEffectOverrides
  , newGLKNamedEffect
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

-- | Overrides record for @\@protocol GLKNamedEffect@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GLKNamedEffectOverrides = GLKNamedEffectOverrides
  { _prepareToDraw :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGLKNamedEffectOverrides :: GLKNamedEffectOverrides
defaultGLKNamedEffectOverrides = GLKNamedEffectOverrides
  { _prepareToDraw = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE glkNamedEffectDelegateClass #-}
glkNamedEffectDelegateClass :: Class
glkNamedEffectDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGLKNamedEffect" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_prepareToDraw = unSelector (mkSelector "prepareToDraw")
  -- prepareToDraw
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GLKNamedEffectOverrides
    case _prepareToDraw rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "prepareToDraw" "v@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GLKNamedEffectOverrides
    if queriedSel == sel_prepareToDraw then pure (maybe 0 (const 1) (_prepareToDraw rec_))
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
newGLKNamedEffect :: GLKNamedEffectOverrides -> IO RawId
newGLKNamedEffect overrides = do
  inst <- class_createInstance glkNamedEffectDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
