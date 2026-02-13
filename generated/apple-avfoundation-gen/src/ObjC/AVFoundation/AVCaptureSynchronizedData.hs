{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSynchronizedData
--
-- An abstract base class representing the data delivered by a data output through the AVCaptureDataOutputSynchronizer interface.
--
-- AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedData: delegate callback delivers a dictionary of key/value pairs, with the keys being the AVCaptureOutput instances returning data, and the values being concrete subclasses of AVCaptureSynchronizedData.
--
-- Generated bindings for @AVCaptureSynchronizedData@.
module ObjC.AVFoundation.AVCaptureSynchronizedData
  ( AVCaptureSynchronizedData
  , IsAVCaptureSynchronizedData(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSynchronizedData avCaptureSynchronizedData => avCaptureSynchronizedData -> IO (Id AVCaptureSynchronizedData)
init_ avCaptureSynchronizedData =
  sendOwnedMessage avCaptureSynchronizedData initSelector

-- | @+ new@
new :: IO (Id AVCaptureSynchronizedData)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSynchronizedData"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureSynchronizedData)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureSynchronizedData)
newSelector = mkSelector "new"

