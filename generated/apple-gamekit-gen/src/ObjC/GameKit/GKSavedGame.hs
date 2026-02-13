{-# LANGUAGE DataKinds #-}
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
  , deviceNameSelector
  , loadDataWithCompletionHandlerSelector
  , modificationDateSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Asynchronously load the data for this saved game. The completion handler is called with loaded data or an error.
--
-- ObjC selector: @- loadDataWithCompletionHandler:@
loadDataWithCompletionHandler :: IsGKSavedGame gkSavedGame => gkSavedGame -> Ptr () -> IO ()
loadDataWithCompletionHandler gkSavedGame handler =
  sendMessage gkSavedGame loadDataWithCompletionHandlerSelector handler

-- | @- name@
name :: IsGKSavedGame gkSavedGame => gkSavedGame -> IO (Id NSString)
name gkSavedGame =
  sendMessage gkSavedGame nameSelector

-- | @- deviceName@
deviceName :: IsGKSavedGame gkSavedGame => gkSavedGame -> IO (Id NSString)
deviceName gkSavedGame =
  sendMessage gkSavedGame deviceNameSelector

-- | @- modificationDate@
modificationDate :: IsGKSavedGame gkSavedGame => gkSavedGame -> IO (Id NSDate)
modificationDate gkSavedGame =
  sendMessage gkSavedGame modificationDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadDataWithCompletionHandler:@
loadDataWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadDataWithCompletionHandlerSelector = mkSelector "loadDataWithCompletionHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @deviceName@
deviceNameSelector :: Selector '[] (Id NSString)
deviceNameSelector = mkSelector "deviceName"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

