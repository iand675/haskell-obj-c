{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFetchedResultsControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSFetchedResultsControllerDelegate defaultNSFetchedResultsControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreData.Delegate.NSFetchedResultsControllerDelegate
  ( NSFetchedResultsControllerDelegateOverrides(..)
  , defaultNSFetchedResultsControllerDelegateOverrides
  , newNSFetchedResultsControllerDelegate
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

-- | Overrides record for @\@protocol NSFetchedResultsControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFetchedResultsControllerDelegateOverrides = NSFetchedResultsControllerDelegateOverrides
  { _controller_didChangeContentWithSnapshot :: !(Maybe (RawId -> RawId -> IO ()))
  , _controller_didChangeContentWithDifference :: !(Maybe (RawId -> RawId -> IO ()))
  , _controllerWillChangeContent :: !(Maybe (RawId -> IO ()))
  , _controllerDidChangeContent :: !(Maybe (RawId -> IO ()))
  , _controller_sectionIndexTitleForSectionName :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFetchedResultsControllerDelegateOverrides :: NSFetchedResultsControllerDelegateOverrides
defaultNSFetchedResultsControllerDelegateOverrides = NSFetchedResultsControllerDelegateOverrides
  { _controller_didChangeContentWithSnapshot = Nothing
  , _controller_didChangeContentWithDifference = Nothing
  , _controllerWillChangeContent = Nothing
  , _controllerDidChangeContent = Nothing
  , _controller_sectionIndexTitleForSectionName = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFetchedResultsControllerDelegateDelegateClass #-}
nsFetchedResultsControllerDelegateDelegateClass :: Class
nsFetchedResultsControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFetchedResultsControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_controller_didChangeContentWithSnapshot = unSelector (mkSelector "controller:didChangeContentWithSnapshot:")
      sel_controller_didChangeContentWithDifference = unSelector (mkSelector "controller:didChangeContentWithDifference:")
      sel_controllerWillChangeContent = unSelector (mkSelector "controllerWillChangeContent:")
      sel_controllerDidChangeContent = unSelector (mkSelector "controllerDidChangeContent:")
      sel_controller_sectionIndexTitleForSectionName = unSelector (mkSelector "controller:sectionIndexTitleForSectionName:")
  -- controller:didChangeContentWithSnapshot:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsControllerDelegateOverrides
    case _controller_didChangeContentWithSnapshot rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:didChangeContentWithSnapshot:" "v@:@@" stub_0

  -- controller:didChangeContentWithDifference:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsControllerDelegateOverrides
    case _controller_didChangeContentWithDifference rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "controller:didChangeContentWithDifference:" "v@:@@" stub_1

  -- controllerWillChangeContent:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsControllerDelegateOverrides
    case _controllerWillChangeContent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "controllerWillChangeContent:" "v@:@" stub_2

  -- controllerDidChangeContent:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsControllerDelegateOverrides
    case _controllerDidChangeContent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "controllerDidChangeContent:" "v@:@" stub_3

  -- controller:sectionIndexTitleForSectionName:
  stub_4 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsControllerDelegateOverrides
    case _controller_sectionIndexTitleForSectionName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "controller:sectionIndexTitleForSectionName:" "@@:@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsControllerDelegateOverrides
    if queriedSel == sel_controller_didChangeContentWithSnapshot then pure (maybe 0 (const 1) (_controller_didChangeContentWithSnapshot rec_))
    else if queriedSel == sel_controller_didChangeContentWithDifference then pure (maybe 0 (const 1) (_controller_didChangeContentWithDifference rec_))
    else if queriedSel == sel_controllerWillChangeContent then pure (maybe 0 (const 1) (_controllerWillChangeContent rec_))
    else if queriedSel == sel_controllerDidChangeContent then pure (maybe 0 (const 1) (_controllerDidChangeContent rec_))
    else if queriedSel == sel_controller_sectionIndexTitleForSectionName then pure (maybe 0 (const 1) (_controller_sectionIndexTitleForSectionName rec_))
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
newNSFetchedResultsControllerDelegate :: NSFetchedResultsControllerDelegateOverrides -> IO RawId
newNSFetchedResultsControllerDelegate overrides = do
  inst <- class_createInstance nsFetchedResultsControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
