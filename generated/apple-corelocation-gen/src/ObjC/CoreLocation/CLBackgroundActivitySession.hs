{-# LANGUAGE DataKinds #-}
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
  , backgroundActivitySessionSelector
  , backgroundActivitySessionWithQueue_handlerSelector
  , initSelector
  , invalidateSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLBackgroundActivitySession clBackgroundActivitySession => clBackgroundActivitySession -> IO (Id CLBackgroundActivitySession)
init_ clBackgroundActivitySession =
  sendOwnedMessage clBackgroundActivitySession initSelector

-- | @+ new@
new :: IO (Id CLBackgroundActivitySession)
new  =
  do
    cls' <- getRequiredClass "CLBackgroundActivitySession"
    sendOwnedClassMessage cls' newSelector

-- | @- invalidate@
invalidate :: IsCLBackgroundActivitySession clBackgroundActivitySession => clBackgroundActivitySession -> IO ()
invalidate clBackgroundActivitySession =
  sendMessage clBackgroundActivitySession invalidateSelector

-- | @+ backgroundActivitySession@
backgroundActivitySession :: IO (Id CLBackgroundActivitySession)
backgroundActivitySession  =
  do
    cls' <- getRequiredClass "CLBackgroundActivitySession"
    sendClassMessage cls' backgroundActivitySessionSelector

-- | @+ backgroundActivitySessionWithQueue:handler:@
backgroundActivitySessionWithQueue_handler :: IsNSObject queue => queue -> Ptr () -> IO (Id CLBackgroundActivitySession)
backgroundActivitySessionWithQueue_handler queue handler =
  do
    cls' <- getRequiredClass "CLBackgroundActivitySession"
    sendClassMessage cls' backgroundActivitySessionWithQueue_handlerSelector (toNSObject queue) handler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLBackgroundActivitySession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLBackgroundActivitySession)
newSelector = mkSelector "new"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @backgroundActivitySession@
backgroundActivitySessionSelector :: Selector '[] (Id CLBackgroundActivitySession)
backgroundActivitySessionSelector = mkSelector "backgroundActivitySession"

-- | @Selector@ for @backgroundActivitySessionWithQueue:handler:@
backgroundActivitySessionWithQueue_handlerSelector :: Selector '[Id NSObject, Ptr ()] (Id CLBackgroundActivitySession)
backgroundActivitySessionWithQueue_handlerSelector = mkSelector "backgroundActivitySessionWithQueue:handler:"

