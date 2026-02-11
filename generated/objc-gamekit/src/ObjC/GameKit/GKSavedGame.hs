{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a saved game for the local player, or a version of a saved game when in conflict
--
-- Generated bindings for @GKSavedGame@.
module ObjC.GameKit.GKSavedGame
  ( GKSavedGame
  , IsGKSavedGame(..)
  , loadDataWithCompletionHandler
  , name
  , deviceName
  , modificationDate
  , loadDataWithCompletionHandlerSelector
  , nameSelector
  , deviceNameSelector
  , modificationDateSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Asynchronously load the data for this saved game. The completion handler is called with loaded data or an error.
--
-- ObjC selector: @- loadDataWithCompletionHandler:@
loadDataWithCompletionHandler :: IsGKSavedGame gkSavedGame => gkSavedGame -> Ptr () -> IO ()
loadDataWithCompletionHandler gkSavedGame  handler =
  sendMsg gkSavedGame (mkSelector "loadDataWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- name@
name :: IsGKSavedGame gkSavedGame => gkSavedGame -> IO (Id NSString)
name gkSavedGame  =
  sendMsg gkSavedGame (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deviceName@
deviceName :: IsGKSavedGame gkSavedGame => gkSavedGame -> IO (Id NSString)
deviceName gkSavedGame  =
  sendMsg gkSavedGame (mkSelector "deviceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modificationDate@
modificationDate :: IsGKSavedGame gkSavedGame => gkSavedGame -> IO (Id NSDate)
modificationDate gkSavedGame  =
  sendMsg gkSavedGame (mkSelector "modificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadDataWithCompletionHandler:@
loadDataWithCompletionHandlerSelector :: Selector
loadDataWithCompletionHandlerSelector = mkSelector "loadDataWithCompletionHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @deviceName@
deviceNameSelector :: Selector
deviceNameSelector = mkSelector "deviceName"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector
modificationDateSelector = mkSelector "modificationDate"

