{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegateSelector
  , initWithMachPortSelector
  , initWithMachPort_optionsSelector
  , machPortSelector
  , portWithMachPortSelector
  , portWithMachPort_optionsSelector
  , removeFromRunLoop_forModeSelector
  , scheduleInRunLoop_forModeSelector
  , setDelegateSelector

  -- * Enum types
  , NSMachPortOptions(NSMachPortOptions)
  , pattern NSMachPortDeallocateNone
  , pattern NSMachPortDeallocateSendRight
  , pattern NSMachPortDeallocateReceiveRight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ portWithMachPort:@
portWithMachPort :: CUInt -> IO (Id NSPort)
portWithMachPort machPort =
  do
    cls' <- getRequiredClass "NSMachPort"
    sendClassMessage cls' portWithMachPortSelector machPort

-- | @- initWithMachPort:@
initWithMachPort :: IsNSMachPort nsMachPort => nsMachPort -> CUInt -> IO (Id NSMachPort)
initWithMachPort nsMachPort machPort =
  sendOwnedMessage nsMachPort initWithMachPortSelector machPort

-- | @- setDelegate:@
setDelegate :: IsNSMachPort nsMachPort => nsMachPort -> RawId -> IO ()
setDelegate nsMachPort anObject =
  sendMessage nsMachPort setDelegateSelector anObject

-- | @- delegate@
delegate :: IsNSMachPort nsMachPort => nsMachPort -> IO RawId
delegate nsMachPort =
  sendMessage nsMachPort delegateSelector

-- | @+ portWithMachPort:options:@
portWithMachPort_options :: CUInt -> NSMachPortOptions -> IO (Id NSPort)
portWithMachPort_options machPort f =
  do
    cls' <- getRequiredClass "NSMachPort"
    sendClassMessage cls' portWithMachPort_optionsSelector machPort f

-- | @- initWithMachPort:options:@
initWithMachPort_options :: IsNSMachPort nsMachPort => nsMachPort -> CUInt -> NSMachPortOptions -> IO (Id NSMachPort)
initWithMachPort_options nsMachPort machPort f =
  sendOwnedMessage nsMachPort initWithMachPort_optionsSelector machPort f

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSMachPort nsMachPort, IsNSRunLoop runLoop, IsNSString mode) => nsMachPort -> runLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsMachPort runLoop mode =
  sendMessage nsMachPort scheduleInRunLoop_forModeSelector (toNSRunLoop runLoop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSMachPort nsMachPort, IsNSRunLoop runLoop, IsNSString mode) => nsMachPort -> runLoop -> mode -> IO ()
removeFromRunLoop_forMode nsMachPort runLoop mode =
  sendMessage nsMachPort removeFromRunLoop_forModeSelector (toNSRunLoop runLoop) (toNSString mode)

-- | @- machPort@
machPort :: IsNSMachPort nsMachPort => nsMachPort -> IO CUInt
machPort nsMachPort =
  sendMessage nsMachPort machPortSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @portWithMachPort:@
portWithMachPortSelector :: Selector '[CUInt] (Id NSPort)
portWithMachPortSelector = mkSelector "portWithMachPort:"

-- | @Selector@ for @initWithMachPort:@
initWithMachPortSelector :: Selector '[CUInt] (Id NSMachPort)
initWithMachPortSelector = mkSelector "initWithMachPort:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @portWithMachPort:options:@
portWithMachPort_optionsSelector :: Selector '[CUInt, NSMachPortOptions] (Id NSPort)
portWithMachPort_optionsSelector = mkSelector "portWithMachPort:options:"

-- | @Selector@ for @initWithMachPort:options:@
initWithMachPort_optionsSelector :: Selector '[CUInt, NSMachPortOptions] (Id NSMachPort)
initWithMachPort_optionsSelector = mkSelector "initWithMachPort:options:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @machPort@
machPortSelector :: Selector '[] CUInt
machPortSelector = mkSelector "machPort"

