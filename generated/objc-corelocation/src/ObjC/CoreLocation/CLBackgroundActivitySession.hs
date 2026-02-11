{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLBackgroundActivitySession@.
module ObjC.CoreLocation.CLBackgroundActivitySession
  ( CLBackgroundActivitySession
  , IsCLBackgroundActivitySession(..)
  , init_
  , new
  , invalidate
  , backgroundActivitySession
  , backgroundActivitySessionWithQueue_handler
  , initSelector
  , newSelector
  , invalidateSelector
  , backgroundActivitySessionSelector
  , backgroundActivitySessionWithQueue_handlerSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLBackgroundActivitySession clBackgroundActivitySession => clBackgroundActivitySession -> IO (Id CLBackgroundActivitySession)
init_ clBackgroundActivitySession  =
  sendMsg clBackgroundActivitySession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLBackgroundActivitySession)
new  =
  do
    cls' <- getRequiredClass "CLBackgroundActivitySession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- invalidate@
invalidate :: IsCLBackgroundActivitySession clBackgroundActivitySession => clBackgroundActivitySession -> IO ()
invalidate clBackgroundActivitySession  =
  sendMsg clBackgroundActivitySession (mkSelector "invalidate") retVoid []

-- | @+ backgroundActivitySession@
backgroundActivitySession :: IO (Id CLBackgroundActivitySession)
backgroundActivitySession  =
  do
    cls' <- getRequiredClass "CLBackgroundActivitySession"
    sendClassMsg cls' (mkSelector "backgroundActivitySession") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ backgroundActivitySessionWithQueue:handler:@
backgroundActivitySessionWithQueue_handler :: IsNSObject queue => queue -> Ptr () -> IO (Id CLBackgroundActivitySession)
backgroundActivitySessionWithQueue_handler queue handler =
  do
    cls' <- getRequiredClass "CLBackgroundActivitySession"
    withObjCPtr queue $ \raw_queue ->
      sendClassMsg cls' (mkSelector "backgroundActivitySessionWithQueue:handler:") (retPtr retVoid) [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @backgroundActivitySession@
backgroundActivitySessionSelector :: Selector
backgroundActivitySessionSelector = mkSelector "backgroundActivitySession"

-- | @Selector@ for @backgroundActivitySessionWithQueue:handler:@
backgroundActivitySessionWithQueue_handlerSelector :: Selector
backgroundActivitySessionWithQueue_handlerSelector = mkSelector "backgroundActivitySessionWithQueue:handler:"

