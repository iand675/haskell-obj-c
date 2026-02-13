{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to launch your app in the background to execute a health research task for studies a user has opted into and that can take minutes to complete.
--
-- Generated bindings for @BGHealthResearchTaskRequest@.
module ObjC.BackgroundTasks.BGHealthResearchTaskRequest
  ( BGHealthResearchTaskRequest
  , IsBGHealthResearchTaskRequest(..)
  , protectionTypeOfRequiredData
  , setProtectionTypeOfRequiredData
  , protectionTypeOfRequiredDataSelector
  , setProtectionTypeOfRequiredDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A String indicating file protection availability required for processing.
--
-- Update this property to indicate what type of data needs to be accessible when the task is run. The default value is @NSFileProtectionCompleteUntilFirstUserAuthentication@
--
-- ObjC selector: @- protectionTypeOfRequiredData@
protectionTypeOfRequiredData :: IsBGHealthResearchTaskRequest bgHealthResearchTaskRequest => bgHealthResearchTaskRequest -> IO (Id NSString)
protectionTypeOfRequiredData bgHealthResearchTaskRequest =
  sendMessage bgHealthResearchTaskRequest protectionTypeOfRequiredDataSelector

-- | A String indicating file protection availability required for processing.
--
-- Update this property to indicate what type of data needs to be accessible when the task is run. The default value is @NSFileProtectionCompleteUntilFirstUserAuthentication@
--
-- ObjC selector: @- setProtectionTypeOfRequiredData:@
setProtectionTypeOfRequiredData :: (IsBGHealthResearchTaskRequest bgHealthResearchTaskRequest, IsNSString value) => bgHealthResearchTaskRequest -> value -> IO ()
setProtectionTypeOfRequiredData bgHealthResearchTaskRequest value =
  sendMessage bgHealthResearchTaskRequest setProtectionTypeOfRequiredDataSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @protectionTypeOfRequiredData@
protectionTypeOfRequiredDataSelector :: Selector '[] (Id NSString)
protectionTypeOfRequiredDataSelector = mkSelector "protectionTypeOfRequiredData"

-- | @Selector@ for @setProtectionTypeOfRequiredData:@
setProtectionTypeOfRequiredDataSelector :: Selector '[Id NSString] ()
setProtectionTypeOfRequiredDataSelector = mkSelector "setProtectionTypeOfRequiredData:"

