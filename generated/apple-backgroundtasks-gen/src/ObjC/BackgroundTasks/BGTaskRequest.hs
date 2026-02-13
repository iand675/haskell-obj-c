{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract class that represents a request for the app to be launched in the background to perform work. Do not instantiate instances of this class directly. Instead, use one of its concrete subclasses.
--
-- Generated bindings for @BGTaskRequest@.
module ObjC.BackgroundTasks.BGTaskRequest
  ( BGTaskRequest
  , IsBGTaskRequest(..)
  , init_
  , new
  , identifier
  , earliestBeginDate
  , setEarliestBeginDate
  , earliestBeginDateSelector
  , identifierSelector
  , initSelector
  , newSelector
  , setEarliestBeginDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBGTaskRequest bgTaskRequest => bgTaskRequest -> IO (Id BGTaskRequest)
init_ bgTaskRequest =
  sendOwnedMessage bgTaskRequest initSelector

-- | @+ new@
new :: IO (Id BGTaskRequest)
new  =
  do
    cls' <- getRequiredClass "BGTaskRequest"
    sendOwnedClassMessage cls' newSelector

-- | The identifier of the task associated with the request.
--
-- ObjC selector: @- identifier@
identifier :: IsBGTaskRequest bgTaskRequest => bgTaskRequest -> IO (Id NSString)
identifier bgTaskRequest =
  sendMessage bgTaskRequest identifierSelector

-- | The earliest date and time at which to run the task.
--
-- Specify @nil@ for no start delay.
--
-- Setting the property indicates that the background task shouldn’t start any earlier than this date. However, the system doesn’t guarantee launching the task at the specified date, but only that it won’t begin sooner.
--
-- ObjC selector: @- earliestBeginDate@
earliestBeginDate :: IsBGTaskRequest bgTaskRequest => bgTaskRequest -> IO (Id NSDate)
earliestBeginDate bgTaskRequest =
  sendMessage bgTaskRequest earliestBeginDateSelector

-- | The earliest date and time at which to run the task.
--
-- Specify @nil@ for no start delay.
--
-- Setting the property indicates that the background task shouldn’t start any earlier than this date. However, the system doesn’t guarantee launching the task at the specified date, but only that it won’t begin sooner.
--
-- ObjC selector: @- setEarliestBeginDate:@
setEarliestBeginDate :: (IsBGTaskRequest bgTaskRequest, IsNSDate value) => bgTaskRequest -> value -> IO ()
setEarliestBeginDate bgTaskRequest value =
  sendMessage bgTaskRequest setEarliestBeginDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BGTaskRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BGTaskRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @earliestBeginDate@
earliestBeginDateSelector :: Selector '[] (Id NSDate)
earliestBeginDateSelector = mkSelector "earliestBeginDate"

-- | @Selector@ for @setEarliestBeginDate:@
setEarliestBeginDateSelector :: Selector '[Id NSDate] ()
setEarliestBeginDateSelector = mkSelector "setEarliestBeginDate:"

