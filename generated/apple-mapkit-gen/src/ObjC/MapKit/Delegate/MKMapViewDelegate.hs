{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MKMapViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMKMapViewDelegate defaultMKMapViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MapKit.Delegate.MKMapViewDelegate
  ( MKMapViewDelegateOverrides(..)
  , defaultMKMapViewDelegateOverrides
  , newMKMapViewDelegate
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

-- | Overrides record for @\@protocol MKMapViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MKMapViewDelegateOverrides = MKMapViewDelegateOverrides
  { _mapView_regionWillChangeAnimated :: !(Maybe (RawId -> Bool -> IO ()))
  , _mapView_regionDidChangeAnimated :: !(Maybe (RawId -> Bool -> IO ()))
  , _mapViewDidChangeVisibleRegion :: !(Maybe (RawId -> IO ()))
  , _mapViewWillStartLoadingMap :: !(Maybe (RawId -> IO ()))
  , _mapViewDidFinishLoadingMap :: !(Maybe (RawId -> IO ()))
  , _mapViewDidFailLoadingMap_withError :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapViewWillStartRenderingMap :: !(Maybe (RawId -> IO ()))
  , _mapViewDidFinishRenderingMap_fullyRendered :: !(Maybe (RawId -> Bool -> IO ()))
  , _mapView_viewForAnnotation :: !(Maybe (RawId -> RawId -> IO RawId))
  , _mapView_didAddAnnotationViews :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_didSelectAnnotationView :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_didDeselectAnnotationView :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_didSelectAnnotation :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_didDeselectAnnotation :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_selectionAccessoryForAnnotation :: !(Maybe (RawId -> RawId -> IO RawId))
  , _mapViewWillStartLocatingUser :: !(Maybe (RawId -> IO ()))
  , _mapViewDidStopLocatingUser :: !(Maybe (RawId -> IO ()))
  , _mapView_didUpdateUserLocation :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_didFailToLocateUserWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_rendererForOverlay :: !(Maybe (RawId -> RawId -> IO RawId))
  , _mapView_didAddOverlayRenderers :: !(Maybe (RawId -> RawId -> IO ()))
  , _mapView_clusterAnnotationForMemberAnnotations :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMKMapViewDelegateOverrides :: MKMapViewDelegateOverrides
defaultMKMapViewDelegateOverrides = MKMapViewDelegateOverrides
  { _mapView_regionWillChangeAnimated = Nothing
  , _mapView_regionDidChangeAnimated = Nothing
  , _mapViewDidChangeVisibleRegion = Nothing
  , _mapViewWillStartLoadingMap = Nothing
  , _mapViewDidFinishLoadingMap = Nothing
  , _mapViewDidFailLoadingMap_withError = Nothing
  , _mapViewWillStartRenderingMap = Nothing
  , _mapViewDidFinishRenderingMap_fullyRendered = Nothing
  , _mapView_viewForAnnotation = Nothing
  , _mapView_didAddAnnotationViews = Nothing
  , _mapView_didSelectAnnotationView = Nothing
  , _mapView_didDeselectAnnotationView = Nothing
  , _mapView_didSelectAnnotation = Nothing
  , _mapView_didDeselectAnnotation = Nothing
  , _mapView_selectionAccessoryForAnnotation = Nothing
  , _mapViewWillStartLocatingUser = Nothing
  , _mapViewDidStopLocatingUser = Nothing
  , _mapView_didUpdateUserLocation = Nothing
  , _mapView_didFailToLocateUserWithError = Nothing
  , _mapView_rendererForOverlay = Nothing
  , _mapView_didAddOverlayRenderers = Nothing
  , _mapView_clusterAnnotationForMemberAnnotations = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mkMapViewDelegateDelegateClass #-}
mkMapViewDelegateDelegateClass :: Class
mkMapViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMKMapViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_mapView_regionWillChangeAnimated = unSelector (mkSelector "mapView:regionWillChangeAnimated:")
      sel_mapView_regionDidChangeAnimated = unSelector (mkSelector "mapView:regionDidChangeAnimated:")
      sel_mapViewDidChangeVisibleRegion = unSelector (mkSelector "mapViewDidChangeVisibleRegion:")
      sel_mapViewWillStartLoadingMap = unSelector (mkSelector "mapViewWillStartLoadingMap:")
      sel_mapViewDidFinishLoadingMap = unSelector (mkSelector "mapViewDidFinishLoadingMap:")
      sel_mapViewDidFailLoadingMap_withError = unSelector (mkSelector "mapViewDidFailLoadingMap:withError:")
      sel_mapViewWillStartRenderingMap = unSelector (mkSelector "mapViewWillStartRenderingMap:")
      sel_mapViewDidFinishRenderingMap_fullyRendered = unSelector (mkSelector "mapViewDidFinishRenderingMap:fullyRendered:")
      sel_mapView_viewForAnnotation = unSelector (mkSelector "mapView:viewForAnnotation:")
      sel_mapView_didAddAnnotationViews = unSelector (mkSelector "mapView:didAddAnnotationViews:")
      sel_mapView_didSelectAnnotationView = unSelector (mkSelector "mapView:didSelectAnnotationView:")
      sel_mapView_didDeselectAnnotationView = unSelector (mkSelector "mapView:didDeselectAnnotationView:")
      sel_mapView_didSelectAnnotation = unSelector (mkSelector "mapView:didSelectAnnotation:")
      sel_mapView_didDeselectAnnotation = unSelector (mkSelector "mapView:didDeselectAnnotation:")
      sel_mapView_selectionAccessoryForAnnotation = unSelector (mkSelector "mapView:selectionAccessoryForAnnotation:")
      sel_mapViewWillStartLocatingUser = unSelector (mkSelector "mapViewWillStartLocatingUser:")
      sel_mapViewDidStopLocatingUser = unSelector (mkSelector "mapViewDidStopLocatingUser:")
      sel_mapView_didUpdateUserLocation = unSelector (mkSelector "mapView:didUpdateUserLocation:")
      sel_mapView_didFailToLocateUserWithError = unSelector (mkSelector "mapView:didFailToLocateUserWithError:")
      sel_mapView_rendererForOverlay = unSelector (mkSelector "mapView:rendererForOverlay:")
      sel_mapView_didAddOverlayRenderers = unSelector (mkSelector "mapView:didAddOverlayRenderers:")
      sel_mapView_clusterAnnotationForMemberAnnotations = unSelector (mkSelector "mapView:clusterAnnotationForMemberAnnotations:")
  -- mapView:regionWillChangeAnimated:
  stub_0 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_regionWillChangeAnimated rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "mapView:regionWillChangeAnimated:" "v@:@B" stub_0

  -- mapView:regionDidChangeAnimated:
  stub_1 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_regionDidChangeAnimated rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "mapView:regionDidChangeAnimated:" "v@:@B" stub_1

  -- mapViewDidChangeVisibleRegion:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewDidChangeVisibleRegion rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapViewDidChangeVisibleRegion:" "v@:@" stub_2

  -- mapViewWillStartLoadingMap:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewWillStartLoadingMap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapViewWillStartLoadingMap:" "v@:@" stub_3

  -- mapViewDidFinishLoadingMap:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewDidFinishLoadingMap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapViewDidFinishLoadingMap:" "v@:@" stub_4

  -- mapViewDidFailLoadingMap:withError:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewDidFailLoadingMap_withError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapViewDidFailLoadingMap:withError:" "v@:@@" stub_5

  -- mapViewWillStartRenderingMap:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewWillStartRenderingMap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapViewWillStartRenderingMap:" "v@:@" stub_6

  -- mapViewDidFinishRenderingMap:fullyRendered:
  stub_7 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewDidFinishRenderingMap_fullyRendered rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "mapViewDidFinishRenderingMap:fullyRendered:" "v@:@B" stub_7

  -- mapView:viewForAnnotation:
  stub_8 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_viewForAnnotation rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "mapView:viewForAnnotation:" "@@:@@" stub_8

  -- mapView:didAddAnnotationViews:
  stub_9 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didAddAnnotationViews rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didAddAnnotationViews:" "v@:@@" stub_9

  -- mapView:didSelectAnnotationView:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didSelectAnnotationView rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didSelectAnnotationView:" "v@:@@" stub_10

  -- mapView:didDeselectAnnotationView:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didDeselectAnnotationView rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didDeselectAnnotationView:" "v@:@@" stub_11

  -- mapView:didSelectAnnotation:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didSelectAnnotation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didSelectAnnotation:" "v@:@@" stub_12

  -- mapView:didDeselectAnnotation:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didDeselectAnnotation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didDeselectAnnotation:" "v@:@@" stub_13

  -- mapView:selectionAccessoryForAnnotation:
  stub_14 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_selectionAccessoryForAnnotation rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "mapView:selectionAccessoryForAnnotation:" "@@:@@" stub_14

  -- mapViewWillStartLocatingUser:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewWillStartLocatingUser rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapViewWillStartLocatingUser:" "v@:@" stub_15

  -- mapViewDidStopLocatingUser:
  stub_16 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapViewDidStopLocatingUser rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapViewDidStopLocatingUser:" "v@:@" stub_16

  -- mapView:didUpdateUserLocation:
  stub_17 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didUpdateUserLocation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didUpdateUserLocation:" "v@:@@" stub_17

  -- mapView:didFailToLocateUserWithError:
  stub_18 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didFailToLocateUserWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didFailToLocateUserWithError:" "v@:@@" stub_18

  -- mapView:rendererForOverlay:
  stub_19 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_rendererForOverlay rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "mapView:rendererForOverlay:" "@@:@@" stub_19

  -- mapView:didAddOverlayRenderers:
  stub_20 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_didAddOverlayRenderers rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "mapView:didAddOverlayRenderers:" "v@:@@" stub_20

  -- mapView:clusterAnnotationForMemberAnnotations:
  stub_21 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    case _mapView_clusterAnnotationForMemberAnnotations rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "mapView:clusterAnnotationForMemberAnnotations:" "@@:@@" stub_21

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapViewDelegateOverrides
    if queriedSel == sel_mapView_regionWillChangeAnimated then pure (maybe 0 (const 1) (_mapView_regionWillChangeAnimated rec_))
    else if queriedSel == sel_mapView_regionDidChangeAnimated then pure (maybe 0 (const 1) (_mapView_regionDidChangeAnimated rec_))
    else if queriedSel == sel_mapViewDidChangeVisibleRegion then pure (maybe 0 (const 1) (_mapViewDidChangeVisibleRegion rec_))
    else if queriedSel == sel_mapViewWillStartLoadingMap then pure (maybe 0 (const 1) (_mapViewWillStartLoadingMap rec_))
    else if queriedSel == sel_mapViewDidFinishLoadingMap then pure (maybe 0 (const 1) (_mapViewDidFinishLoadingMap rec_))
    else if queriedSel == sel_mapViewDidFailLoadingMap_withError then pure (maybe 0 (const 1) (_mapViewDidFailLoadingMap_withError rec_))
    else if queriedSel == sel_mapViewWillStartRenderingMap then pure (maybe 0 (const 1) (_mapViewWillStartRenderingMap rec_))
    else if queriedSel == sel_mapViewDidFinishRenderingMap_fullyRendered then pure (maybe 0 (const 1) (_mapViewDidFinishRenderingMap_fullyRendered rec_))
    else if queriedSel == sel_mapView_viewForAnnotation then pure (maybe 0 (const 1) (_mapView_viewForAnnotation rec_))
    else if queriedSel == sel_mapView_didAddAnnotationViews then pure (maybe 0 (const 1) (_mapView_didAddAnnotationViews rec_))
    else if queriedSel == sel_mapView_didSelectAnnotationView then pure (maybe 0 (const 1) (_mapView_didSelectAnnotationView rec_))
    else if queriedSel == sel_mapView_didDeselectAnnotationView then pure (maybe 0 (const 1) (_mapView_didDeselectAnnotationView rec_))
    else if queriedSel == sel_mapView_didSelectAnnotation then pure (maybe 0 (const 1) (_mapView_didSelectAnnotation rec_))
    else if queriedSel == sel_mapView_didDeselectAnnotation then pure (maybe 0 (const 1) (_mapView_didDeselectAnnotation rec_))
    else if queriedSel == sel_mapView_selectionAccessoryForAnnotation then pure (maybe 0 (const 1) (_mapView_selectionAccessoryForAnnotation rec_))
    else if queriedSel == sel_mapViewWillStartLocatingUser then pure (maybe 0 (const 1) (_mapViewWillStartLocatingUser rec_))
    else if queriedSel == sel_mapViewDidStopLocatingUser then pure (maybe 0 (const 1) (_mapViewDidStopLocatingUser rec_))
    else if queriedSel == sel_mapView_didUpdateUserLocation then pure (maybe 0 (const 1) (_mapView_didUpdateUserLocation rec_))
    else if queriedSel == sel_mapView_didFailToLocateUserWithError then pure (maybe 0 (const 1) (_mapView_didFailToLocateUserWithError rec_))
    else if queriedSel == sel_mapView_rendererForOverlay then pure (maybe 0 (const 1) (_mapView_rendererForOverlay rec_))
    else if queriedSel == sel_mapView_didAddOverlayRenderers then pure (maybe 0 (const 1) (_mapView_didAddOverlayRenderers rec_))
    else if queriedSel == sel_mapView_clusterAnnotationForMemberAnnotations then pure (maybe 0 (const 1) (_mapView_clusterAnnotationForMemberAnnotations rec_))
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
newMKMapViewDelegate :: MKMapViewDelegateOverrides -> IO RawId
newMKMapViewDelegate overrides = do
  inst <- class_createInstance mkMapViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
