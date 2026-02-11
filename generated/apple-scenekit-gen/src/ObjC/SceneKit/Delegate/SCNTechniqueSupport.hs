{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNTechniqueSupport@.
--
-- Usage:
--
-- @
-- delegate <- newSCNTechniqueSupport defaultSCNTechniqueSupportOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNTechniqueSupport
  ( SCNTechniqueSupportOverrides(..)
  , defaultSCNTechniqueSupportOverrides
  , newSCNTechniqueSupport
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

-- | Overrides record for @\@protocol SCNTechniqueSupport@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNTechniqueSupportOverrides = SCNTechniqueSupportOverrides
  { _technique :: !(Maybe (IO RawId))
  , _setTechnique :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNTechniqueSupportOverrides :: SCNTechniqueSupportOverrides
defaultSCNTechniqueSupportOverrides = SCNTechniqueSupportOverrides
  { _technique = Nothing
  , _setTechnique = Nothing
  }

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
{-# NOINLINE scnTechniqueSupportDelegateClass #-}
scnTechniqueSupportDelegateClass :: Class
scnTechniqueSupportDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNTechniqueSupport" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_technique = unSelector (mkSelector "technique")
      sel_setTechnique = unSelector (mkSelector "setTechnique:")
  -- technique
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNTechniqueSupportOverrides
    case _technique rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "technique" "@@:" stub_0

  -- setTechnique:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNTechniqueSupportOverrides
    case _setTechnique rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setTechnique:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNTechniqueSupportOverrides
    if queriedSel == sel_technique then pure (maybe 0 (const 1) (_technique rec_))
    else if queriedSel == sel_setTechnique then pure (maybe 0 (const 1) (_setTechnique rec_))
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
newSCNTechniqueSupport :: SCNTechniqueSupportOverrides -> IO RawId
newSCNTechniqueSupport overrides = do
  inst <- class_createInstance scnTechniqueSupportDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
