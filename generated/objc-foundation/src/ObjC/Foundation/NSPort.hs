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
  , portSelector
  , invalidateSelector
  , setDelegateSelector
  , delegateSelector
  , scheduleInRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , sendBeforeDate_components_from_reservedSelector
  , sendBeforeDate_msgid_components_from_reservedSelector
  , addConnection_toRunLoop_forModeSelector
  , removeConnection_fromRunLoop_forModeSelector
  , validSelector
  , reservedSpaceLengthSelector


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

-- | @+ port@
port :: IO (Id NSPort)
port  =
  do
    cls' <- getRequiredClass "NSPort"
    sendClassMsg cls' (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- invalidate@
invalidate :: IsNSPort nsPort => nsPort -> IO ()
invalidate nsPort  =
  sendMsg nsPort (mkSelector "invalidate") retVoid []

-- | @- setDelegate:@
setDelegate :: IsNSPort nsPort => nsPort -> RawId -> IO ()
setDelegate nsPort  anObject =
  sendMsg nsPort (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- delegate@
delegate :: IsNSPort nsPort => nsPort -> IO RawId
delegate nsPort  =
  fmap (RawId . castPtr) $ sendMsg nsPort (mkSelector "delegate") (retPtr retVoid) []

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSPort nsPort, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> runLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsPort  runLoop mode =
withObjCPtr runLoop $ \raw_runLoop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsPort (mkSelector "scheduleInRunLoop:forMode:") retVoid [argPtr (castPtr raw_runLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSPort nsPort, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> runLoop -> mode -> IO ()
removeFromRunLoop_forMode nsPort  runLoop mode =
withObjCPtr runLoop $ \raw_runLoop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsPort (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_runLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- sendBeforeDate:components:from:reserved:@
sendBeforeDate_components_from_reserved :: (IsNSPort nsPort, IsNSDate limitDate, IsNSMutableArray components, IsNSPort receivePort) => nsPort -> limitDate -> components -> receivePort -> CULong -> IO Bool
sendBeforeDate_components_from_reserved nsPort  limitDate components receivePort headerSpaceReserved =
withObjCPtr limitDate $ \raw_limitDate ->
  withObjCPtr components $ \raw_components ->
    withObjCPtr receivePort $ \raw_receivePort ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPort (mkSelector "sendBeforeDate:components:from:reserved:") retCULong [argPtr (castPtr raw_limitDate :: Ptr ()), argPtr (castPtr raw_components :: Ptr ()), argPtr (castPtr raw_receivePort :: Ptr ()), argCULong (fromIntegral headerSpaceReserved)]

-- | @- sendBeforeDate:msgid:components:from:reserved:@
sendBeforeDate_msgid_components_from_reserved :: (IsNSPort nsPort, IsNSDate limitDate, IsNSMutableArray components, IsNSPort receivePort) => nsPort -> limitDate -> CULong -> components -> receivePort -> CULong -> IO Bool
sendBeforeDate_msgid_components_from_reserved nsPort  limitDate msgID components receivePort headerSpaceReserved =
withObjCPtr limitDate $ \raw_limitDate ->
  withObjCPtr components $ \raw_components ->
    withObjCPtr receivePort $ \raw_receivePort ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPort (mkSelector "sendBeforeDate:msgid:components:from:reserved:") retCULong [argPtr (castPtr raw_limitDate :: Ptr ()), argCULong (fromIntegral msgID), argPtr (castPtr raw_components :: Ptr ()), argPtr (castPtr raw_receivePort :: Ptr ()), argCULong (fromIntegral headerSpaceReserved)]

-- | @- addConnection:toRunLoop:forMode:@
addConnection_toRunLoop_forMode :: (IsNSPort nsPort, IsNSConnection conn, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> conn -> runLoop -> mode -> IO ()
addConnection_toRunLoop_forMode nsPort  conn runLoop mode =
withObjCPtr conn $ \raw_conn ->
  withObjCPtr runLoop $ \raw_runLoop ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsPort (mkSelector "addConnection:toRunLoop:forMode:") retVoid [argPtr (castPtr raw_conn :: Ptr ()), argPtr (castPtr raw_runLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeConnection:fromRunLoop:forMode:@
removeConnection_fromRunLoop_forMode :: (IsNSPort nsPort, IsNSConnection conn, IsNSRunLoop runLoop, IsNSString mode) => nsPort -> conn -> runLoop -> mode -> IO ()
removeConnection_fromRunLoop_forMode nsPort  conn runLoop mode =
withObjCPtr conn $ \raw_conn ->
  withObjCPtr runLoop $ \raw_runLoop ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsPort (mkSelector "removeConnection:fromRunLoop:forMode:") retVoid [argPtr (castPtr raw_conn :: Ptr ()), argPtr (castPtr raw_runLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- valid@
valid :: IsNSPort nsPort => nsPort -> IO Bool
valid nsPort  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPort (mkSelector "valid") retCULong []

-- | @- reservedSpaceLength@
reservedSpaceLength :: IsNSPort nsPort => nsPort -> IO CULong
reservedSpaceLength nsPort  =
  sendMsg nsPort (mkSelector "reservedSpaceLength") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @sendBeforeDate:components:from:reserved:@
sendBeforeDate_components_from_reservedSelector :: Selector
sendBeforeDate_components_from_reservedSelector = mkSelector "sendBeforeDate:components:from:reserved:"

-- | @Selector@ for @sendBeforeDate:msgid:components:from:reserved:@
sendBeforeDate_msgid_components_from_reservedSelector :: Selector
sendBeforeDate_msgid_components_from_reservedSelector = mkSelector "sendBeforeDate:msgid:components:from:reserved:"

-- | @Selector@ for @addConnection:toRunLoop:forMode:@
addConnection_toRunLoop_forModeSelector :: Selector
addConnection_toRunLoop_forModeSelector = mkSelector "addConnection:toRunLoop:forMode:"

-- | @Selector@ for @removeConnection:fromRunLoop:forMode:@
removeConnection_fromRunLoop_forModeSelector :: Selector
removeConnection_fromRunLoop_forModeSelector = mkSelector "removeConnection:fromRunLoop:forMode:"

-- | @Selector@ for @valid@
validSelector :: Selector
validSelector = mkSelector "valid"

-- | @Selector@ for @reservedSpaceLength@
reservedSpaceLengthSelector :: Selector
reservedSpaceLengthSelector = mkSelector "reservedSpaceLength"

