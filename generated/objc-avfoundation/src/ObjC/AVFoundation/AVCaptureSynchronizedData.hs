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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSynchronizedData avCaptureSynchronizedData => avCaptureSynchronizedData -> IO (Id AVCaptureSynchronizedData)
init_ avCaptureSynchronizedData  =
  sendMsg avCaptureSynchronizedData (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureSynchronizedData)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSynchronizedData"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

