{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNSceneRenderer@.
--
-- Usage:
--
-- @
-- delegate <- newSCNSceneRenderer defaultSCNSceneRendererOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNSceneRenderer
  ( SCNSceneRendererOverrides(..)
  , defaultSCNSceneRendererOverrides
  , newSCNSceneRenderer
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

-- | Overrides record for @\@protocol SCNSceneRenderer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNSceneRendererOverrides = SCNSceneRendererOverrides
  { _isNodeInsideFrustum_withPointOfView :: !(Maybe (RawId -> RawId -> IO Bool))
  , _nodesInsideFrustumWithPointOfView :: !(Maybe (RawId -> IO RawId))
  , _scene :: !(Maybe (IO RawId))
  , _setScene :: !(Maybe (RawId -> IO ()))
  , _sceneTime :: !(Maybe (IO Double))
  , _setSceneTime :: !(Maybe (Double -> IO ()))
  , _delegate :: !(Maybe (IO RawId))
  , _setDelegate :: !(Maybe (RawId -> IO ()))
  , _playing :: !(Maybe (IO Bool))
  , _setPlaying :: !(Maybe (Bool -> IO ()))
  , _loops :: !(Maybe (IO Bool))
  , _setLoops :: !(Maybe (Bool -> IO ()))
  , _pointOfView :: !(Maybe (IO RawId))
  , _setPointOfView :: !(Maybe (RawId -> IO ()))
  , _autoenablesDefaultLighting :: !(Maybe (IO Bool))
  , _setAutoenablesDefaultLighting :: !(Maybe (Bool -> IO ()))
  , _jitteringEnabled :: !(Maybe (IO Bool))
  , _setJitteringEnabled :: !(Maybe (Bool -> IO ()))
  , _temporalAntialiasingEnabled :: !(Maybe (IO Bool))
  , _setTemporalAntialiasingEnabled :: !(Maybe (Bool -> IO ()))
  , _showsStatistics :: !(Maybe (IO Bool))
  , _setShowsStatistics :: !(Maybe (Bool -> IO ()))
  , _overlaySKScene :: !(Maybe (IO RawId))
  , _setOverlaySKScene :: !(Maybe (RawId -> IO ()))
  , _currentRenderCommandEncoder :: !(Maybe (IO RawId))
  , _currentRenderPassDescriptor :: !(Maybe (IO RawId))
  , _device :: !(Maybe (IO RawId))
  , _commandQueue :: !(Maybe (IO RawId))
  , _audioEngine :: !(Maybe (IO RawId))
  , _audioEnvironmentNode :: !(Maybe (IO RawId))
  , _audioListener :: !(Maybe (IO RawId))
  , _setAudioListener :: !(Maybe (RawId -> IO ()))
  , _currentTime :: !(Maybe (IO Double))
  , _setCurrentTime :: !(Maybe (Double -> IO ()))
  , _usesReverseZ :: !(Maybe (IO Bool))
  , _setUsesReverseZ :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNSceneRendererOverrides :: SCNSceneRendererOverrides
defaultSCNSceneRendererOverrides = SCNSceneRendererOverrides
  { _isNodeInsideFrustum_withPointOfView = Nothing
  , _nodesInsideFrustumWithPointOfView = Nothing
  , _scene = Nothing
  , _setScene = Nothing
  , _sceneTime = Nothing
  , _setSceneTime = Nothing
  , _delegate = Nothing
  , _setDelegate = Nothing
  , _playing = Nothing
  , _setPlaying = Nothing
  , _loops = Nothing
  , _setLoops = Nothing
  , _pointOfView = Nothing
  , _setPointOfView = Nothing
  , _autoenablesDefaultLighting = Nothing
  , _setAutoenablesDefaultLighting = Nothing
  , _jitteringEnabled = Nothing
  , _setJitteringEnabled = Nothing
  , _temporalAntialiasingEnabled = Nothing
  , _setTemporalAntialiasingEnabled = Nothing
  , _showsStatistics = Nothing
  , _setShowsStatistics = Nothing
  , _overlaySKScene = Nothing
  , _setOverlaySKScene = Nothing
  , _currentRenderCommandEncoder = Nothing
  , _currentRenderPassDescriptor = Nothing
  , _device = Nothing
  , _commandQueue = Nothing
  , _audioEngine = Nothing
  , _audioEnvironmentNode = Nothing
  , _audioListener = Nothing
  , _setAudioListener = Nothing
  , _currentTime = Nothing
  , _setCurrentTime = Nothing
  , _usesReverseZ = Nothing
  , _setUsesReverseZ = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnSceneRendererDelegateClass #-}
scnSceneRendererDelegateClass :: Class
scnSceneRendererDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNSceneRenderer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_isNodeInsideFrustum_withPointOfView = unSelector (mkSelector "isNodeInsideFrustum:withPointOfView:")
      sel_nodesInsideFrustumWithPointOfView = unSelector (mkSelector "nodesInsideFrustumWithPointOfView:")
      sel_scene = unSelector (mkSelector "scene")
      sel_setScene = unSelector (mkSelector "setScene:")
      sel_sceneTime = unSelector (mkSelector "sceneTime")
      sel_setSceneTime = unSelector (mkSelector "setSceneTime:")
      sel_delegate = unSelector (mkSelector "delegate")
      sel_setDelegate = unSelector (mkSelector "setDelegate:")
      sel_playing = unSelector (mkSelector "playing")
      sel_setPlaying = unSelector (mkSelector "setPlaying:")
      sel_loops = unSelector (mkSelector "loops")
      sel_setLoops = unSelector (mkSelector "setLoops:")
      sel_pointOfView = unSelector (mkSelector "pointOfView")
      sel_setPointOfView = unSelector (mkSelector "setPointOfView:")
      sel_autoenablesDefaultLighting = unSelector (mkSelector "autoenablesDefaultLighting")
      sel_setAutoenablesDefaultLighting = unSelector (mkSelector "setAutoenablesDefaultLighting:")
      sel_jitteringEnabled = unSelector (mkSelector "jitteringEnabled")
      sel_setJitteringEnabled = unSelector (mkSelector "setJitteringEnabled:")
      sel_temporalAntialiasingEnabled = unSelector (mkSelector "temporalAntialiasingEnabled")
      sel_setTemporalAntialiasingEnabled = unSelector (mkSelector "setTemporalAntialiasingEnabled:")
      sel_showsStatistics = unSelector (mkSelector "showsStatistics")
      sel_setShowsStatistics = unSelector (mkSelector "setShowsStatistics:")
      sel_overlaySKScene = unSelector (mkSelector "overlaySKScene")
      sel_setOverlaySKScene = unSelector (mkSelector "setOverlaySKScene:")
      sel_currentRenderCommandEncoder = unSelector (mkSelector "currentRenderCommandEncoder")
      sel_currentRenderPassDescriptor = unSelector (mkSelector "currentRenderPassDescriptor")
      sel_device = unSelector (mkSelector "device")
      sel_commandQueue = unSelector (mkSelector "commandQueue")
      sel_audioEngine = unSelector (mkSelector "audioEngine")
      sel_audioEnvironmentNode = unSelector (mkSelector "audioEnvironmentNode")
      sel_audioListener = unSelector (mkSelector "audioListener")
      sel_setAudioListener = unSelector (mkSelector "setAudioListener:")
      sel_currentTime = unSelector (mkSelector "currentTime")
      sel_setCurrentTime = unSelector (mkSelector "setCurrentTime:")
      sel_usesReverseZ = unSelector (mkSelector "usesReverseZ")
      sel_setUsesReverseZ = unSelector (mkSelector "setUsesReverseZ:")
  -- isNodeInsideFrustum:withPointOfView:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _isNodeInsideFrustum_withPointOfView rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "isNodeInsideFrustum:withPointOfView:" "B@:@@" stub_0

  -- nodesInsideFrustumWithPointOfView:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _nodesInsideFrustumWithPointOfView rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "nodesInsideFrustumWithPointOfView:" "@@:@" stub_1

  -- scene
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _scene rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "scene" "@@:" stub_2

  -- setScene:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setScene:" "v@:@" stub_3

  -- sceneTime
  stub_4 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _sceneTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "sceneTime" "d@:" stub_4

  -- setSceneTime:
  stub_5 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setSceneTime rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setSceneTime:" "v@:d" stub_5

  -- delegate
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _delegate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "delegate" "@@:" stub_6

  -- setDelegate:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setDelegate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDelegate:" "v@:@" stub_7

  -- playing
  stub_8 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _playing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "playing" "B@:" stub_8

  -- setPlaying:
  stub_9 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setPlaying rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setPlaying:" "v@:B" stub_9

  -- loops
  stub_10 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _loops rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "loops" "B@:" stub_10

  -- setLoops:
  stub_11 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setLoops rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setLoops:" "v@:B" stub_11

  -- pointOfView
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _pointOfView rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pointOfView" "@@:" stub_12

  -- setPointOfView:
  stub_13 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setPointOfView rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setPointOfView:" "v@:@" stub_13

  -- autoenablesDefaultLighting
  stub_14 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _autoenablesDefaultLighting rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "autoenablesDefaultLighting" "B@:" stub_14

  -- setAutoenablesDefaultLighting:
  stub_15 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setAutoenablesDefaultLighting rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setAutoenablesDefaultLighting:" "v@:B" stub_15

  -- jitteringEnabled
  stub_16 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _jitteringEnabled rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "jitteringEnabled" "B@:" stub_16

  -- setJitteringEnabled:
  stub_17 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setJitteringEnabled rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setJitteringEnabled:" "v@:B" stub_17

  -- temporalAntialiasingEnabled
  stub_18 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _temporalAntialiasingEnabled rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "temporalAntialiasingEnabled" "B@:" stub_18

  -- setTemporalAntialiasingEnabled:
  stub_19 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setTemporalAntialiasingEnabled rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setTemporalAntialiasingEnabled:" "v@:B" stub_19

  -- showsStatistics
  stub_20 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _showsStatistics rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "showsStatistics" "B@:" stub_20

  -- setShowsStatistics:
  stub_21 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setShowsStatistics rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setShowsStatistics:" "v@:B" stub_21

  -- overlaySKScene
  stub_22 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _overlaySKScene rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "overlaySKScene" "@@:" stub_22

  -- setOverlaySKScene:
  stub_23 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setOverlaySKScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setOverlaySKScene:" "v@:@" stub_23

  -- currentRenderCommandEncoder
  stub_24 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _currentRenderCommandEncoder rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "currentRenderCommandEncoder" "@@:" stub_24

  -- currentRenderPassDescriptor
  stub_25 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _currentRenderPassDescriptor rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "currentRenderPassDescriptor" "@@:" stub_25

  -- device
  stub_26 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_26

  -- commandQueue
  stub_27 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _commandQueue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "commandQueue" "@@:" stub_27

  -- audioEngine
  stub_28 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _audioEngine rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "audioEngine" "@@:" stub_28

  -- audioEnvironmentNode
  stub_29 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _audioEnvironmentNode rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "audioEnvironmentNode" "@@:" stub_29

  -- audioListener
  stub_30 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _audioListener rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "audioListener" "@@:" stub_30

  -- setAudioListener:
  stub_31 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setAudioListener rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAudioListener:" "v@:@" stub_31

  -- currentTime
  stub_32 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _currentTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "currentTime" "d@:" stub_32

  -- setCurrentTime:
  stub_33 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setCurrentTime rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setCurrentTime:" "v@:d" stub_33

  -- usesReverseZ
  stub_34 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _usesReverseZ rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "usesReverseZ" "B@:" stub_34

  -- setUsesReverseZ:
  stub_35 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    case _setUsesReverseZ rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setUsesReverseZ:" "v@:B" stub_35

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererOverrides
    if queriedSel == sel_isNodeInsideFrustum_withPointOfView then pure (maybe 0 (const 1) (_isNodeInsideFrustum_withPointOfView rec_))
    else if queriedSel == sel_nodesInsideFrustumWithPointOfView then pure (maybe 0 (const 1) (_nodesInsideFrustumWithPointOfView rec_))
    else if queriedSel == sel_scene then pure (maybe 0 (const 1) (_scene rec_))
    else if queriedSel == sel_setScene then pure (maybe 0 (const 1) (_setScene rec_))
    else if queriedSel == sel_sceneTime then pure (maybe 0 (const 1) (_sceneTime rec_))
    else if queriedSel == sel_setSceneTime then pure (maybe 0 (const 1) (_setSceneTime rec_))
    else if queriedSel == sel_delegate then pure (maybe 0 (const 1) (_delegate rec_))
    else if queriedSel == sel_setDelegate then pure (maybe 0 (const 1) (_setDelegate rec_))
    else if queriedSel == sel_playing then pure (maybe 0 (const 1) (_playing rec_))
    else if queriedSel == sel_setPlaying then pure (maybe 0 (const 1) (_setPlaying rec_))
    else if queriedSel == sel_loops then pure (maybe 0 (const 1) (_loops rec_))
    else if queriedSel == sel_setLoops then pure (maybe 0 (const 1) (_setLoops rec_))
    else if queriedSel == sel_pointOfView then pure (maybe 0 (const 1) (_pointOfView rec_))
    else if queriedSel == sel_setPointOfView then pure (maybe 0 (const 1) (_setPointOfView rec_))
    else if queriedSel == sel_autoenablesDefaultLighting then pure (maybe 0 (const 1) (_autoenablesDefaultLighting rec_))
    else if queriedSel == sel_setAutoenablesDefaultLighting then pure (maybe 0 (const 1) (_setAutoenablesDefaultLighting rec_))
    else if queriedSel == sel_jitteringEnabled then pure (maybe 0 (const 1) (_jitteringEnabled rec_))
    else if queriedSel == sel_setJitteringEnabled then pure (maybe 0 (const 1) (_setJitteringEnabled rec_))
    else if queriedSel == sel_temporalAntialiasingEnabled then pure (maybe 0 (const 1) (_temporalAntialiasingEnabled rec_))
    else if queriedSel == sel_setTemporalAntialiasingEnabled then pure (maybe 0 (const 1) (_setTemporalAntialiasingEnabled rec_))
    else if queriedSel == sel_showsStatistics then pure (maybe 0 (const 1) (_showsStatistics rec_))
    else if queriedSel == sel_setShowsStatistics then pure (maybe 0 (const 1) (_setShowsStatistics rec_))
    else if queriedSel == sel_overlaySKScene then pure (maybe 0 (const 1) (_overlaySKScene rec_))
    else if queriedSel == sel_setOverlaySKScene then pure (maybe 0 (const 1) (_setOverlaySKScene rec_))
    else if queriedSel == sel_currentRenderCommandEncoder then pure (maybe 0 (const 1) (_currentRenderCommandEncoder rec_))
    else if queriedSel == sel_currentRenderPassDescriptor then pure (maybe 0 (const 1) (_currentRenderPassDescriptor rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_commandQueue then pure (maybe 0 (const 1) (_commandQueue rec_))
    else if queriedSel == sel_audioEngine then pure (maybe 0 (const 1) (_audioEngine rec_))
    else if queriedSel == sel_audioEnvironmentNode then pure (maybe 0 (const 1) (_audioEnvironmentNode rec_))
    else if queriedSel == sel_audioListener then pure (maybe 0 (const 1) (_audioListener rec_))
    else if queriedSel == sel_setAudioListener then pure (maybe 0 (const 1) (_setAudioListener rec_))
    else if queriedSel == sel_currentTime then pure (maybe 0 (const 1) (_currentTime rec_))
    else if queriedSel == sel_setCurrentTime then pure (maybe 0 (const 1) (_setCurrentTime rec_))
    else if queriedSel == sel_usesReverseZ then pure (maybe 0 (const 1) (_usesReverseZ rec_))
    else if queriedSel == sel_setUsesReverseZ then pure (maybe 0 (const 1) (_setUsesReverseZ rec_))
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
newSCNSceneRenderer :: SCNSceneRendererOverrides -> IO RawId
newSCNSceneRenderer overrides = do
  inst <- class_createInstance scnSceneRendererDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
