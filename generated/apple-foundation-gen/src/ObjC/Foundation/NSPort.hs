{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPort@.
module ObjC.Foundation.NSPort
  ( NSPort
  , IsNSPort(..)
  , port
  , invalidate
  , setDelegate
  , delegate
  , scheduleInRunLoop_forMode
  , removeFromRunLoop_forMode
  , sendBeforeDate_components_from_reserved
  , sendBeforeDate_msgid_components_from_reserved
  , addConnection_toRunLoop_forMode
  , removeConnection_fromRunLoop_forMode
  , valid
  , reservedSpaceLength
  , addConnection_toRunLoop_forModeSelector
  , delegateSelector
  , invalidateSelector
  , portSelector
  , removeConnection_fromRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , reservedSpaceLengthSelector
  , scheduleInRunLoop_forModeSelector
  , sendBeforeDate_components_from_reservedSelector
  , sendBeforeDate_msgid_components_from_reservedSelector
  , setDelegateSelector
  , validSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ port@
port :: IO (Id NSPort)
port  =
  do
    cls' <- getRequiredClass "NSPort"
    sendClassMessage cls' portSelector

-- | @- invalidate@
invalidate :: IsNSPort nsPort => nsPort -> IO ()
invalidate nsPort =
  sendMessage nsPort invalidateSelector

-- | @- setDelegate:@
setDelegate :: IsNSPort nsPort => nsPort -> RawId -> IO ()
setDelegate nsPort anObject =
  sendMessage nsPort setDelegateSelector anObject

-- | @- delegate@
delegate :: IsNSPort nsPort => nsPort -> IO RawId
delegate nsPort =
  sendMessage nsPort delegateSelector

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSPort nsPort, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> runLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsPort runLoop mode =
  sendMessage nsPort scheduleInRunLoop_forModeSelector (toNSRunLoop runLoop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSPort nsPort, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> runLoop -> mode -> IO ()
removeFromRunLoop_forMode nsPort runLoop mode =
  sendMessage nsPort removeFromRunLoop_forModeSelector (toNSRunLoop runLoop) (toNSString mode)

-- | @- sendBeforeDate:components:from:reserved:@
sendBeforeDate_components_from_reserved :: (IsNSPort nsPort, IsNSDate limitDate, IsNSMutableArray components, IsNSPort receivePort) => nsPort -> limitDate -> components -> receivePort -> CULong -> IO Bool
sendBeforeDate_components_from_reserved nsPort limitDate components receivePort headerSpaceReserved =
  sendMessage nsPort sendBeforeDate_components_from_reservedSelector (toNSDate limitDate) (toNSMutableArray components) (toNSPort receivePort) headerSpaceReserved

-- | @- sendBeforeDate:msgid:components:from:reserved:@
sendBeforeDate_msgid_components_from_reserved :: (IsNSPort nsPort, IsNSDate limitDate, IsNSMutableArray components, IsNSPort receivePort) => nsPort -> limitDate -> CULong -> components -> receivePort -> CULong -> IO Bool
sendBeforeDate_msgid_components_from_reserved nsPort limitDate msgID components receivePort headerSpaceReserved =
  sendMessage nsPort sendBeforeDate_msgid_components_from_reservedSelector (toNSDate limitDate) msgID (toNSMutableArray components) (toNSPort receivePort) headerSpaceReserved

-- | @- addConnection:toRunLoop:forMode:@
addConnection_toRunLoop_forMode :: (IsNSPort nsPort, IsNSConnection conn, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> conn -> runLoop -> mode -> IO ()
addConnection_toRunLoop_forMode nsPort conn runLoop mode =
  sendMessage nsPort addConnection_toRunLoop_forModeSelector (toNSConnection conn) (toNSRunLoop runLoop) (toNSString mode)

-- | @- removeConnection:fromRunLoop:forMode:@
removeConnection_fromRunLoop_forMode :: (IsNSPort nsPort, IsNSConnection conn, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> conn -> runLoop -> mode -> IO ()
removeConnection_fromRunLoop_forMode nsPort conn runLoop mode =
  sendMessage nsPort removeConnection_fromRunLoop_forModeSelector (toNSConnection conn) (toNSRunLoop runLoop) (toNSString mode)

-- | @- valid@
valid :: IsNSPort nsPort => nsPort -> IO Bool
valid nsPort =
  sendMessage nsPort validSelector

-- | @- reservedSpaceLength@
reservedSpaceLength :: IsNSPort nsPort => nsPort -> IO CULong
reservedSpaceLength nsPort =
  sendMessage nsPort reservedSpaceLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSPort)
portSelector = mkSelector "port"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @sendBeforeDate:components:from:reserved:@
sendBeforeDate_components_from_reservedSelector :: Selector '[Id NSDate, Id NSMutableArray, Id NSPort, CULong] Bool
sendBeforeDate_components_from_reservedSelector = mkSelector "sendBeforeDate:components:from:reserved:"

-- | @Selector@ for @sendBeforeDate:msgid:components:from:reserved:@
sendBeforeDate_msgid_components_from_reservedSelector :: Selector '[Id NSDate, CULong, Id NSMutableArray, Id NSPort, CULong] Bool
sendBeforeDate_msgid_components_from_reservedSelector = mkSelector "sendBeforeDate:msgid:components:from:reserved:"

-- | @Selector@ for @addConnection:toRunLoop:forMode:@
addConnection_toRunLoop_forModeSelector :: Selector '[Id NSConnection, Id NSRunLoop, Id NSString] ()
addConnection_toRunLoop_forModeSelector = mkSelector "addConnection:toRunLoop:forMode:"

-- | @Selector@ for @removeConnection:fromRunLoop:forMode:@
removeConnection_fromRunLoop_forModeSelector :: Selector '[Id NSConnection, Id NSRunLoop, Id NSString] ()
removeConnection_fromRunLoop_forModeSelector = mkSelector "removeConnection:fromRunLoop:forMode:"

-- | @Selector@ for @valid@
validSelector :: Selector '[] Bool
validSelector = mkSelector "valid"

-- | @Selector@ for @reservedSpaceLength@
reservedSpaceLengthSelector :: Selector '[] CULong
reservedSpaceLengthSelector = mkSelector "reservedSpaceLength"

