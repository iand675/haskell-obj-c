{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerApplicationController@.
module ObjC.MediaPlayer.MPMusicPlayerApplicationController
  ( MPMusicPlayerApplicationController
  , IsMPMusicPlayerApplicationController(..)
  , performQueueTransaction_completionHandler
  , performQueueTransaction_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- performQueueTransaction:completionHandler:@
performQueueTransaction_completionHandler :: IsMPMusicPlayerApplicationController mpMusicPlayerApplicationController => mpMusicPlayerApplicationController -> Ptr () -> Ptr () -> IO ()
performQueueTransaction_completionHandler mpMusicPlayerApplicationController queueTransaction completionHandler =
  sendMessage mpMusicPlayerApplicationController performQueueTransaction_completionHandlerSelector queueTransaction completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performQueueTransaction:completionHandler:@
performQueueTransaction_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
performQueueTransaction_completionHandlerSelector = mkSelector "performQueueTransaction:completionHandler:"

