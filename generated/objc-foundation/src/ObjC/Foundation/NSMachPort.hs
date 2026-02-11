{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMachPort@.
module ObjC.Foundation.NSMachPort
  ( NSMachPort
  , IsNSMachPort(..)
  , portWithMachPort
  , initWithMachPort
  , setDelegate
  , delegate
  , portWithMachPort_options
  , initWithMachPort_options
  , scheduleInRunLoop_forMode
  , removeFromRunLoop_forMode
  , machPort
  , portWithMachPortSelector
  , initWithMachPortSelector
  , setDelegateSelector
  , delegateSelector
  , portWithMachPort_optionsSelector
  , initWithMachPort_optionsSelector
  , scheduleInRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , machPortSelector

  -- * Enum types
  , NSMachPortOptions(NSMachPortOptions)
  , pattern NSMachPortDeallocateNone
  , pattern NSMachPortDeallocateSendRight
  , pattern NSMachPortDeallocateReceiveRight

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ portWithMachPort:@
portWithMachPort :: CUInt -> IO (Id NSPort)
portWithMachPort machPort =
  do
    cls' <- getRequiredClass "NSMachPort"
    sendClassMsg cls' (mkSelector "portWithMachPort:") (retPtr retVoid) [argCUInt (fromIntegral machPort)] >>= retainedObject . castPtr

-- | @- initWithMachPort:@
initWithMachPort :: IsNSMachPort nsMachPort => nsMachPort -> CUInt -> IO (Id NSMachPort)
initWithMachPort nsMachPort  machPort =
  sendMsg nsMachPort (mkSelector "initWithMachPort:") (retPtr retVoid) [argCUInt (fromIntegral machPort)] >>= ownedObject . castPtr

-- | @- setDelegate:@
setDelegate :: IsNSMachPort nsMachPort => nsMachPort -> RawId -> IO ()
setDelegate nsMachPort  anObject =
  sendMsg nsMachPort (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- delegate@
delegate :: IsNSMachPort nsMachPort => nsMachPort -> IO RawId
delegate nsMachPort  =
  fmap (RawId . castPtr) $ sendMsg nsMachPort (mkSelector "delegate") (retPtr retVoid) []

-- | @+ portWithMachPort:options:@
portWithMachPort_options :: CUInt -> NSMachPortOptions -> IO (Id NSPort)
portWithMachPort_options machPort f =
  do
    cls' <- getRequiredClass "NSMachPort"
    sendClassMsg cls' (mkSelector "portWithMachPort:options:") (retPtr retVoid) [argCUInt (fromIntegral machPort), argCULong (coerce f)] >>= retainedObject . castPtr

-- | @- initWithMachPort:options:@
initWithMachPort_options :: IsNSMachPort nsMachPort => nsMachPort -> CUInt -> NSMachPortOptions -> IO (Id NSMachPort)
initWithMachPort_options nsMachPort  machPort f =
  sendMsg nsMachPort (mkSelector "initWithMachPort:options:") (retPtr retVoid) [argCUInt (fromIntegral machPort), argCULong (coerce f)] >>= ownedObject . castPtr

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSMachPort nsMachPort, IsNSRunLoop runLoop, IsNSString mode) => nsMachPort -> runLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsMachPort  runLoop mode =
withObjCPtr runLoop $ \raw_runLoop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsMachPort (mkSelector "scheduleInRunLoop:forMode:") retVoid [argPtr (castPtr raw_runLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSMachPort nsMachPort, IsNSRunLoop runLoop, IsNSString mode) => nsMachPort -> runLoop -> mode -> IO ()
removeFromRunLoop_forMode nsMachPort  runLoop mode =
withObjCPtr runLoop $ \raw_runLoop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsMachPort (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_runLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- machPort@
machPort :: IsNSMachPort nsMachPort => nsMachPort -> IO CUInt
machPort nsMachPort  =
  sendMsg nsMachPort (mkSelector "machPort") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @portWithMachPort:@
portWithMachPortSelector :: Selector
portWithMachPortSelector = mkSelector "portWithMachPort:"

-- | @Selector@ for @initWithMachPort:@
initWithMachPortSelector :: Selector
initWithMachPortSelector = mkSelector "initWithMachPort:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @portWithMachPort:options:@
portWithMachPort_optionsSelector :: Selector
portWithMachPort_optionsSelector = mkSelector "portWithMachPort:options:"

-- | @Selector@ for @initWithMachPort:options:@
initWithMachPort_optionsSelector :: Selector
initWithMachPort_optionsSelector = mkSelector "initWithMachPort:options:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @machPort@
machPortSelector :: Selector
machPortSelector = mkSelector "machPort"

