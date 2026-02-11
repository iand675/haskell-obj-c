{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSDraggingInfo@.
--
-- Usage:
--
-- @
-- delegate <- newNSDraggingInfo defaultNSDraggingInfoOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSDraggingInfo
  ( NSDraggingInfoOverrides(..)
  , defaultNSDraggingInfoOverrides
  , newNSDraggingInfo
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

-- | Overrides record for @\@protocol NSDraggingInfo@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSDraggingInfoOverrides = NSDraggingInfoOverrides
  { _namesOfPromisedFilesDroppedAtDestination :: !(Maybe (RawId -> IO RawId))
  , _resetSpringLoading :: !(Maybe (IO ()))
  , _draggingDestinationWindow :: !(Maybe (IO RawId))
  , _draggedImage :: !(Maybe (IO RawId))
  , _draggingPasteboard :: !(Maybe (IO RawId))
  , _draggingSource :: !(Maybe (IO RawId))
  , _draggingSequenceNumber :: !(Maybe (IO Int))
  , _animatesToDestination :: !(Maybe (IO Bool))
  , _setAnimatesToDestination :: !(Maybe (Bool -> IO ()))
  , _numberOfValidItemsForDrop :: !(Maybe (IO Int))
  , _setNumberOfValidItemsForDrop :: !(Maybe (Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSDraggingInfoOverrides :: NSDraggingInfoOverrides
defaultNSDraggingInfoOverrides = NSDraggingInfoOverrides
  { _namesOfPromisedFilesDroppedAtDestination = Nothing
  , _resetSpringLoading = Nothing
  , _draggingDestinationWindow = Nothing
  , _draggedImage = Nothing
  , _draggingPasteboard = Nothing
  , _draggingSource = Nothing
  , _draggingSequenceNumber = Nothing
  , _animatesToDestination = Nothing
  , _setAnimatesToDestination = Nothing
  , _numberOfValidItemsForDrop = Nothing
  , _setNumberOfValidItemsForDrop = Nothing
  }

foreign import ccall "wrapper"
  wrap_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsDraggingInfoDelegateClass #-}
nsDraggingInfoDelegateClass :: Class
nsDraggingInfoDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSDraggingInfo" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_namesOfPromisedFilesDroppedAtDestination = unSelector (mkSelector "namesOfPromisedFilesDroppedAtDestination:")
      sel_resetSpringLoading = unSelector (mkSelector "resetSpringLoading")
      sel_draggingDestinationWindow = unSelector (mkSelector "draggingDestinationWindow")
      sel_draggedImage = unSelector (mkSelector "draggedImage")
      sel_draggingPasteboard = unSelector (mkSelector "draggingPasteboard")
      sel_draggingSource = unSelector (mkSelector "draggingSource")
      sel_draggingSequenceNumber = unSelector (mkSelector "draggingSequenceNumber")
      sel_animatesToDestination = unSelector (mkSelector "animatesToDestination")
      sel_setAnimatesToDestination = unSelector (mkSelector "setAnimatesToDestination:")
      sel_numberOfValidItemsForDrop = unSelector (mkSelector "numberOfValidItemsForDrop")
      sel_setNumberOfValidItemsForDrop = unSelector (mkSelector "setNumberOfValidItemsForDrop:")
  -- namesOfPromisedFilesDroppedAtDestination:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _namesOfPromisedFilesDroppedAtDestination rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "namesOfPromisedFilesDroppedAtDestination:" "@@:@" stub_0

  -- resetSpringLoading
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _resetSpringLoading rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "resetSpringLoading" "v@:" stub_1

  -- draggingDestinationWindow
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _draggingDestinationWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "draggingDestinationWindow" "@@:" stub_2

  -- draggedImage
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _draggedImage rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "draggedImage" "@@:" stub_3

  -- draggingPasteboard
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _draggingPasteboard rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "draggingPasteboard" "@@:" stub_4

  -- draggingSource
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _draggingSource rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "draggingSource" "@@:" stub_5

  -- draggingSequenceNumber
  stub_6 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _draggingSequenceNumber rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "draggingSequenceNumber" "q@:" stub_6

  -- animatesToDestination
  stub_7 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _animatesToDestination rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "animatesToDestination" "B@:" stub_7

  -- setAnimatesToDestination:
  stub_8 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _setAnimatesToDestination rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAnimatesToDestination:" "v@:B" stub_8

  -- numberOfValidItemsForDrop
  stub_9 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _numberOfValidItemsForDrop rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "numberOfValidItemsForDrop" "q@:" stub_9

  -- setNumberOfValidItemsForDrop:
  stub_10 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    case _setNumberOfValidItemsForDrop rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setNumberOfValidItemsForDrop:" "v@:q" stub_10

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDraggingInfoOverrides
    if queriedSel == sel_namesOfPromisedFilesDroppedAtDestination then pure (maybe 0 (const 1) (_namesOfPromisedFilesDroppedAtDestination rec_))
    else if queriedSel == sel_resetSpringLoading then pure (maybe 0 (const 1) (_resetSpringLoading rec_))
    else if queriedSel == sel_draggingDestinationWindow then pure (maybe 0 (const 1) (_draggingDestinationWindow rec_))
    else if queriedSel == sel_draggedImage then pure (maybe 0 (const 1) (_draggedImage rec_))
    else if queriedSel == sel_draggingPasteboard then pure (maybe 0 (const 1) (_draggingPasteboard rec_))
    else if queriedSel == sel_draggingSource then pure (maybe 0 (const 1) (_draggingSource rec_))
    else if queriedSel == sel_draggingSequenceNumber then pure (maybe 0 (const 1) (_draggingSequenceNumber rec_))
    else if queriedSel == sel_animatesToDestination then pure (maybe 0 (const 1) (_animatesToDestination rec_))
    else if queriedSel == sel_setAnimatesToDestination then pure (maybe 0 (const 1) (_setAnimatesToDestination rec_))
    else if queriedSel == sel_numberOfValidItemsForDrop then pure (maybe 0 (const 1) (_numberOfValidItemsForDrop rec_))
    else if queriedSel == sel_setNumberOfValidItemsForDrop then pure (maybe 0 (const 1) (_setNumberOfValidItemsForDrop rec_))
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
newNSDraggingInfo :: NSDraggingInfoOverrides -> IO RawId
newNSDraggingInfo overrides = do
  inst <- class_createInstance nsDraggingInfoDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
